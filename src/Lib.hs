{-# LANGUAGE AllowAmbiguousTypes #-}

module Lib where

import Control.Eff
import Control.Monad
import Data.OpenUnion (Member)
import Data.Traversable

import Interfaces

process
  :: forall readhandle r. (Member Env r, Member (FileReader readhandle) r)
  => Eff r ()
process = do
  printString "\n"
  args <- getArgs
  let fileName = head args
  fileHandle <- openFile @readhandle fileName
  fileContents <- readFileContent fileHandle
  printString $ "File '" ++ fileName ++ "': " ++ fileContents
