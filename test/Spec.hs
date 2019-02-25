import Lib

import Control.Eff

import Implementations
import TestImplementations

main :: IO ()
main =
  ( runLift
  . runTestFileReader "aaa"
  . runTestEnv ["file.txt"]
  ) (process @String)