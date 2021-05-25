{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Import
import Options.Applicative.Simple
import qualified Paths_quad
import Quad
import Quad.Docker

main :: IO ()
main = do
  (options, ()) <-
    simpleOptions
      $(simpleVersion Paths_quad.version)
      "Header for command line arguments"
      "Program description, also for command line arguments"
      ( Options
          <$> switch
            ( long "verbose"
                <> short 'v'
                <> help "Verbose output?"
            )
      )
      empty
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  -- the default docker socket is at /var/run/docker.sock
  manager <- newUnixManager "/var/run/docker.sock"
  withLogFunc lo $ \lf ->
    let app =
          App
            { appLogFunc = lf,
              appProcessContext = pc,
              appOptions = options,
              appHttpManager = manager
            }
     in runRIO app run
