import Lib

import Control.Eff

import Implementations
import TestImplementations

main :: IO ()
main =
  ( runLift
  . runFileReader
  . runTestEnv ["file.txt"]
  ) process