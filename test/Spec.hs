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
          , testAddingLimitOrders

          , bidAskSetAfterAddingLimitOrders
          , addingInvalidLimitOrders
          , limitOrdersAtSamePriceLevel

          , testTakeBuyMarketOrder
          , testExecuteBuyMarketOrder
          , testTakeSellMarketOrder
          , testExecuteSellMarketOrder
          ]

-- ---------------------------------------------------------------------- 
-- Unit Tests
-- ---------------------------------------------------------------------- 

testEmptyOrderBook :: TestTree
testEmptyOrderBook = testCaseSteps "Test empty order book" $ \step -> do 
  step "Action: Instantiate empty order book"
  let ob = mkEmptyOrderBook

  step "Assert: obLimitOrders is empty"
  Map.size (obLimitOrders ob) @?= 0
  
  step "Assert: obMarketOrders is empty"
  null (obMarketOrders ob) @? "Wrong obMarketOrders"

  step "Assert: obLastPrice is Nothing"
  isNothing (obLastPrice ob) @? "Wrong obLastPrice"

  step "Assert: obCurBid is Nothing"
  isNothing (obCurBid ob) @? "Wrong obCurBid"

  step "Assert: obCurAsk is Nothing"
  isNothing (obCurAsk ob) @? "Wrong obCurAsk"

  step "Assert: obIncrement > 0"
  obIncrement ob > 0 @? "Wrong obIncrement"

testAddingLimitOrders :: TestTree
testAddingLimitOrders = testCaseSteps "Test adding a limit order" $ \step -> do
  let ob1 = mkEmptyOrderBook 
      -- BUY @10 of Asset @ $0.90
      l1  = mkLimitOrder "pkh1" 1_000 90 Buy   
      ob2 = addLimitOrder l1 ob1

  step "Assert: Order book has 1 limit order"
  Map.size (obLimitOrders ob2) @?= 1

  step "Assert: Order book curBid is 90"
  obCurBid ob2 @?= Just 90

  let -- SELL @10 of Asset @ $1.10
      l2  = mkLimitOrder "pkh2" 1_000 110 Sell 
      ob3 = addLimitOrder l2 ob2

  step "Assert: Order book has 2 limit orders"
  Map.size (obLimitOrders ob3) @?= 2

  step "Assert: Order book curAsk is 110"
  obCurAsk ob3 @?= Just 110
 
-- Test: Bid/Ask price is updated after adding a limit order
bidAskSetAfterAddingLimitOrders :: TestTree
bidAskSetAfterAddingLimitOrders = 
  testCaseSteps "Check bid/ask prices after adding limit orders" $ \step -> do 
    let ob1 = mkEmptyOrderBook
        lb1 = mkLimitOrder "pkh1" 1_000 90 Buy    -- BUY $10 @ $0.90
        ls1 = mkLimitOrder "pkh2" 2_000 110 Sell  -- SELL $20 of Asset @ $1.10
        ob2 = addLimitOrders [lb1, ls1] ob1       -- Add limit orders

    step "Assert: Bid is set correctly (90)"
    obCurBid ob2 @?= Just 90

    step "Assert: Ask is set correctly (110)"
    obCurAsk ob2 @?= Just 110

    let lb2 = mkLimitOrder "pkh3" 5_000 80 Buy    -- BUY $5 of Asset @ $0.80
        ls2 = mkLimitOrder "pkh4" 5_000 100 Sell  -- SELL $5 of Asset @ $1
        ob3 = addLimitOrders [lb2, ls2] ob2       -- Add limit orders

    step "Assert: Bid is set correctly (90)"
    obCurBid ob3 @?= Just 90

    step "Assert: Ask is set correctly (100)"
    obCurAsk ob3 @?= Just 100

    step "Assert: Order book limit order length (4)"
    length (obLimitOrders ob3) @?= 4

addingInvalidLimitOrders :: TestTree 
addingInvalidLimitOrders = 
  testCaseSteps "Check adding invalid limit orders" $ \step -> do
    let ob1  = mkEmptyOrderBook
        lb1 = mkLimitOrder "pkh1" 1_000 80 Buy
        ls1 = mkLimitOrder "pkh2" 1_000 120 Sell
        ob2 = addLimitOrders [lb1, ls1] ob1

    obCurBid ob2 @?= Just 80
    obCurAsk ob2 @?= Just 120 

    let lb2 = mkLimitOrder "pkh3" 1_000 120 Buy  -- invalid
        ls2 = mkLimitOrder "pkh3" 1_000 70  Sell -- invalid
        ob3 = addLimitOrders [lb2, ls2] ob2

    step "Assert: Order book unchanged (invalid orders rejected)"
    ob3 @?= ob2

limitOrdersAtSamePriceLevel :: TestTree
limitOrdersAtSamePriceLevel = 
  testCaseSteps "Check adding limit orders at same price level" $ \step -> do 
    let ob1  = mkEmptyOrderBook
        lb1 = mkLimitOrder "pkh1" 1_000 80 Buy
        lb2 = mkLimitOrder "pkh2" 1_000 80 Buy
        ob2 = addLimitOrders [lb1, lb2] ob1

    step "Assert: Order book has 2 limit orders (2 BUY)"
    length (getOrdersAtKey 80 (obLimitOrders ob2)) @?= 2
    length (getFlattenedBuyOrders ob2)             @?= 2
    length (getFlattenedOrders ob2)                @?= 2

    let ls1 = mkLimitOrder "pkh3" 1_000 110 Sell  
        ls2 = mkLimitOrder "pkh4" 1_000 110 Sell  
        ob3 = addLimitOrders [ls1, ls2] ob2

    step "Assert: Order book as 4 limit orders (2 BUY, 2 SELL)"
    length (getOrdersAtKey 110 (obLimitOrders ob3)) @?= 2
    length (getFlattenedSellOrders ob3)             @?= 2
    length (getFlattenedBuyOrders ob3)              @?= 2
    length (getFlattenedOrders ob3)                 @?= 4

-- Test takeOrdersForBuy
testTakeBuyMarketOrder :: TestTree 
testTakeBuyMarketOrder = 
  testCaseSteps "Simple BUY market order" $ \step -> do 
    let -- SELL 10xAsset at $1.00 (value @10)
        lb1         = mkLimitOrder "pkh1" 10 100 Sell  
        ob          = addLimitOrder lb1 mkEmptyOrderBook 
        Just curAsk = obCurAsk ob
        (accV, accOs, newOb) = takeOrdersForBuy curAsk 1000 ob (0, []) 

    step "Assert: 1000 consumed value"
    accV @?= 1_000

    step "Assert: 1 consumed order"
    length accOs @?= 1

    step "Assert: Empty order book"
    Map.null (obLimitOrders newOb) @? "Order book not empty"

-- Test takeOrdersForSell
testTakeSellMarketOrder :: TestTree 
testTakeSellMarketOrder = 
  testCaseSteps "Simple SELL market order" $ \step -> do 
    let -- BUY $10 of Asset at $1.00 (value @10)
        lb1         = mkLimitOrder "pkh1" 1_000 100 Buy  
        ob          = addLimitOrder lb1 mkEmptyOrderBook 
        Just curBid = obCurBid ob
        (accAmt, accOs, newOb) = takeOrdersForSell curBid 10 ob (0, []) 
    
    step "Assert: 10 consumed amount"
    accAmt @?= 10

    step "Assert: 1 consumed order"
    length accOs @?= 1

    step "Assert: Empty order book"
    Map.null (obLimitOrders newOb) @? "Order book not empty" 

testExecuteBuyMarketOrder :: TestTree
testExecuteBuyMarketOrder = 
  testCaseSteps "Simple execute BUY market order" $ \step -> do
    let -- SELL 10xAsset at $1.00 (value @10)
        ls1 = mkLimitOrder "pkh1" 10 100 Sell  
        ob1 = addLimitOrder ls1 mkEmptyOrderBook 
    
    let -- BUY $10 of Asset
        mo  = mkMarketOrder "pkh2" 1_000 Buy 
        ob2 = executeMarketOrder mo ob1 

    step "Assert: Order book empty"
    null (getFlattenedOrders ob2) @? "Order book not empty"
    
testExecuteSellMarketOrder :: TestTree
testExecuteSellMarketOrder = 
  testCaseSteps "Simple execute SELL market order" $ \step -> do
    let -- BUY $10 of Asset at $1.00 (value 10xAsset)
        lb1 = mkLimitOrder "pkh1" 1_000 100 Buy 
        ob1 = addLimitOrder lb1 mkEmptyOrderBook 
    
    let -- SELL 10xAsset
        mo  = mkMarketOrder "pkh2" 10 Sell 
        ob2 = executeMarketOrder mo ob1 

    step "Assert: Order book empty"
    null (getFlattenedOrders ob2) @? "Order book not empty"

