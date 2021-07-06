{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Quad (run) where

import Quad.Types (App)
import RIO

run :: RIO App ()
run = do
  logInfo "We're inside the application!"
