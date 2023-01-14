{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores  #-}

module Main where

import Control.Exception
import Test.Tasty
import Test.Tasty.HUnit
import System.Exit (ExitCode(ExitSuccess))
import System.Environment

import Data.Map qualified as Map
import Data.Maybe (isNothing, isJust)

import OrderBook.Model
import OrderBook.Matching

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

-- ---------------------------------------------------------------------- 
-- Test Tree
-- ---------------------------------------------------------------------- 

tests :: TestTree
tests = testGroup "Tests" 
          [ testEmptyOrderBook
          , testAddingLimitOrders1
          ]

-- ---------------------------------------------------------------------- 
-- Unit Tests
-- ---------------------------------------------------------------------- 

testEmptyOrderBook :: TestTree
testEmptyOrderBook = testCaseSteps "Test empty order book" $ \step -> do 
  step "Action: Instantiate empty order book"
  let ob = mkEmptyOrderBook

  step "Assert: obLimitOrders is empty"
  assertBool "Wrong obLimitOrders" $ Map.size (obLimitOrders ob) == 0
  step "Assert: obMarketOrders is empty"
  assertBool "Wrong obMarketOrders" $ null (obMarketOrders ob)

  step "Assert: obLastPrice is Nothing"
  assertBool "Wrong obLastPrice" $ isNothing (obLastPrice ob)
  step "Assert: obCurBid is Nothing"
  assertBool "Wrong obCurBid" $ isNothing (obCurBid ob)
  step "Assert: obCurAsk is Nothing"
  assertBool "Wrong obCurAsk" $ isNothing (obCurAsk ob)

  step "Assert: obIncrement > 0"
  assertBool "Wrong obIncrement" $ obIncrement ob > 0

testAddingLimitOrders1 :: TestTree
testAddingLimitOrders1 = testCaseSteps "Test adding a limit order" $ \step -> do
  step "Action: Instantiate empty order book"
  let ob1 = mkEmptyOrderBook 

  step "Action: Make limit order l1 (Buy [$10 /Asset] @ $0.90)"
  let l1 = mkLimitOrder "pkh1" 1_000 90 Buy 

  step "Action: Add l1 to order book"
  let ob2 = addLimitOrder l1 ob1

  step "Assert: Order book has 1 limit order"
  assertBool "Wrong obLimitOrders" $ Map.size (obLimitOrders ob2) == 1

  step "Assert: Order book curBid is 90"
  assertBool "Wrong obCurBid" $ obCurBid ob2 == Just 90

  step "Action: Make limit order l1 (Sell [$10 /Asset] @$1.10)"
  let l2 = mkLimitOrder "pkh2" 1_000 110 Sell

  step "Action: Add l2 to order book"
  let ob3 = addLimitOrder l2 ob2

  step "Assert: Order book has 2 limit orders"
  assertBool "Wrong obLimitOrders" $ Map.size (obLimitOrders ob3) == 2

  step "Assert: Order book curAsk is 110"
  assertBool "Wrond obCurAsk" $ obCurAsk ob3 == Just 110
 
-- TODO: Add 5 limit orders and check the bid/ask and order book

-- TODO: After executing a market order, either the bid or ask is updated,
--       not both.

-- ---------------------------------------------------------------------- 
-- Reference
-- ---------------------------------------------------------------------- 

unitTests :: TestTree
unitTests = testGroup "Unit Tests"
  [ testCase "List comparison (different length)" $
      [1, 2, 3] `compare` [1, 2] @?= GT
  
  , testCase "List comparison (same length)" $
      [1, 2, 3] `compare` [1, 2, 2] @?= LT

  , testCase "2+2=4" $
      2+2 @?= 4

  , testCase "7 is even" $ 
      assertBool "Oops, 7 is odd" (even 7)
  ]
