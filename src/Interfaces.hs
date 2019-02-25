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


data FileReader readhandle a where
  OpenFile :: String -> FileReader readhandle readhandle
  ReadFileContent :: readhandle -> FileReader readhandle String

openFile :: Member (FileReader readhandle) r => String -> Eff r readhandle
openFile = send . OpenFile

readFileContent :: Member (FileReader readhandle) r => readhandle -> Eff r String
readFileContent = send . ReadFileContent
