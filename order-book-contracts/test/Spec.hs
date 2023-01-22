{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Test.Tasty
import Spec.Trace qualified as Trace

main :: IO ()
main = defaultMain Trace.tests
