module Interfaces where

import Control.Eff
import Control.Eff.Extend
import Data.OpenUnion (Member)
import System.IO as SystemIO

data Env a where
  GetArgs :: Env [String]
  PrintString :: String -> Env ()

getArgs :: Member Env r => Eff r [String]
getArgs = send GetArgs

printString :: Member Env r => String -> Eff r ()
printString = send . PrintString


data family FileHandle h

data FileReader a where
  OpenFile :: String -> FileReader (FileHandle h)
  ReadFileContent :: (FileHandle b)-> FileReader String

openFile :: Member FileReader r => String -> Eff r (FileHandle h)
openFile = send . OpenFile

readFileContent :: Member FileReader r => (FileHandle h) -> Eff r String
readFileContent = send . ReadFileContent
