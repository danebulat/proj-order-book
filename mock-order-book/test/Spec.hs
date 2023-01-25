{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores  #-}

module Main where

import Control.Exception
import Test.Tasty
import Test.Tasty.HUnit
import System.Exit (ExitCode(ExitSuccess))
import System.Environment

import Spec.BasicOrderBookTests qualified as BasicTests
import Spec.LargeOrderBookTests qualified as LargeTests

-- ---------------------------------------------------------------------- 
-- Test Tree
-- ---------------------------------------------------------------------- 

tests :: TestTree 
tests = testGroup "Order book Tests" 
          [ BasicTests.tests 
          , LargeTests.tests 
          ]

-- ---------------------------------------------------------------------- 
-- Main
-- ---------------------------------------------------------------------- 

main :: IO ()
main = do 
  setEnv "TASTY_TIMEOUT"        "40s"
  setEnv "TASTY_COLOR"          "always"
  setEnv "TASTY_HIDE_SUCCESSES" "false"

  defaultMain tests `catch` 
    (\e -> do 
      if e == ExitSuccess
        then putStrLn "Exited successfully"
        else putStrLn "Error occured"
      throwIO e)

  unsetEnv "TASTY_TIMEOUT"
  unsetEnv "TASTY_COLOR"
  unsetEnv "TASTY_HIDE_SUCCESSES"

