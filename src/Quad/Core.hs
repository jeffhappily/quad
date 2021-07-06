module Quad.Core where

import Quad.Docker (
  ContainerExitCode (..),
  CreateContainerOptions (CreateContainerOptions),
  Image,
  createContainer,
  startContainer,
 )
import Quad.Types (QuadM)
import RIO
import qualified RIO.List as List
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
  { name :: StepName
  , commands :: NonEmpty Text
  , image :: Image
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

-- | A CI build, consisting of a pipeline, the build state and the completed steps
data Build = Build
  { pipeline :: Pipeline
  , state :: BuildState
  , completedSteps :: Map StepName StepResult
  }
  deriving (Eq, Show)

-- | Return @StepResult@ according to the exit code
exitCodeToStepResult :: ContainerExitCode -> StepResult
exitCodeToStepResult exit =
  if exitCode exit == 0
    then StepSucceeded
    else StepFailed exit

{- | Stepping the build forward, i.e., run the next step if available or end the build
 if failed
-}
progress :: Build -> QuadM Build
progress build =
  case build.state of
    BuildReady ->
      case buildHasNextStep build of
        Left res ->
          pure build{state = BuildFinished res}
        Right step -> do
          let options = CreateContainerOptions step.image
          container <- createContainer options
          startContainer container

          let s = BuildRunningState (name step)
          pure build{state = BuildRunning s}
    BuildRunning state ->
      -- Assume step run successfully
      let exit = ContainerExitCode 0
          result = exitCodeToStepResult exit
       in pure $
            build
              { state = BuildReady
              , completedSteps = Map.insert state.step result build.completedSteps
              }
    BuildFinished _ -> pure build

{- | Find the next step for the build, return either @BuildResult@ (build has finished)
 or the next step to run
-}
buildHasNextStep :: Build -> Either BuildResult Step
buildHasNextStep build =
  if allSucceeded
    then case nextStep of
      Just step -> Right step
      Nothing -> Left BuildSucceeded
    else Left BuildFailed
 where
  allSucceeded = List.all (== StepSucceeded) build.completedSteps
  nextStep = List.find notRunned build.pipeline.steps
  notRunned step = not $ Map.member step.name build.completedSteps
