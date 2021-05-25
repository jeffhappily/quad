{-# LANGUAGE NoImplicitPrelude #-}

module Quad.Types where

import qualified Network.HTTP.Client as Http
import RIO
import RIO.Process

type QuadM = RIO App

-- | Command line arguments
data Options = Options
  { optionsVerbose :: !Bool
  }

data App = App
  { appLogFunc :: !LogFunc,
    appProcessContext :: !ProcessContext,
    appOptions :: !Options,
    appHttpManager :: !Http.Manager
  }

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x {appLogFunc = y})

instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x {appProcessContext = y})
