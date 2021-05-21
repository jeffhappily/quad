module Quad.Core where

import Import
import Quad.Docker (ContainerExitCode (..), Image)
import RIO.List
import qualified RIO.Map as Map

newtype StepName = StepName
  { stepNameText :: Text
  }
  deriving (Eq, Show, Ord)

-- | Pipeline consists of many @Step@s
newtype Pipeline = Pipeline
  { steps :: NonEmpty Step
  }
  deriving (Eq, Show)

-- | A list of commands to run in @image@
data Step = Step
  { name :: StepName,
    commands :: NonEmpty Text,
    image :: Image
  }
  deriving (Eq, Show)

newtype BuildRunningState = BuildRunningState
  { step :: StepName
  }
  deriving (Eq, Show)

data BuildResult
  = BuildSucceeded
  | BuildFailed
  deriving (Eq, Show)

data BuildState
  = BuildReady
  | BuildRunning BuildRunningState
  | BuildFinished BuildResult
  deriving (Eq, Show)

data StepResult
  = StepFailed ContainerExitCode
  | StepSucceeded
  deriving (Eq, Show)

-- | A CI build, consisting of a pipeline and the build state
data Build = Build
  { pipeline :: Pipeline,
    state :: BuildState,
    completedSteps :: Map StepName StepResult
  }
  deriving (Eq, Show)

exitCodeToStepResult :: ContainerExitCode -> StepResult
exitCodeToStepResult exit =
  if exitCode exit == 0
    then StepSucceeded
    else StepFailed exit

progress :: Build -> IO Build
progress build@Build {..} =
  case state of
    BuildReady ->
      case buildHasNextStep build of
        Left res ->
          pure $ build {state = BuildFinished res}
        Right step ->
          let s = BuildRunningState (name step)
           in pure $ build {state = BuildRunning s}
    BuildRunning BuildRunningState {..} ->
      -- Assume step run successfully
      let exit = ContainerExitCode 0
          result = exitCodeToStepResult exit
       in pure $
            build
              { state = BuildReady,
                completedSteps = Map.insert step result completedSteps
              }
    BuildFinished _ -> pure build

buildHasNextStep :: Build -> Either BuildResult Step
buildHasNextStep Build {..} =
  if allSucceeded
    then case nextStep of
      Just step -> Right step
      Nothing -> Left BuildSucceeded
    else Left BuildFailed
  where
    allSucceeded = all (== StepSucceeded) completedSteps
    nextStep = find notRunned (steps pipeline)
    notRunned Step {..} = not $ Map.member name completedSteps