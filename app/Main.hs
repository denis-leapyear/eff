module Main where

import Control.Eff
import System.IO as SystemIO

import Lib
import Implementations

main :: IO ()
main =
  ( runLift
  . runFileReader
  . runEnv
  ) (process @SystemIO.Handle)
