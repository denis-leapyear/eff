module Main where

import Control.Eff

import Lib
import Implementations

main :: IO ()
main = (runLift . runFileReader . runEnv) process
