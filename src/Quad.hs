{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Quad (run) where

import Import
import Quad.Types

run :: RIO App ()
run = do
  logInfo "We're inside the application!"
