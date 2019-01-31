module Lib where

import Control.Eff
import Control.Monad
import Data.OpenUnion (Member)
import Data.Traversable

import Interfaces

process :: (Member Env r, Member FileReader r) => Eff r ()
process = do
  printString "\n"
  args <- getArgs
  let fileName = head args
  fileHandle <- openFile fileName
  fileContents <- readFileContent fileHandle
  printString $ "File '" ++ fileName ++ "': " ++ fileContents
