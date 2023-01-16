{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores  #-}

module Main where

import Control.Exception
import Test.Tasty
import Test.Tasty.HUnit
import System.Exit (ExitCode(ExitSuccess))
import System.Environment

import Data.Map qualified as Map
import Data.Maybe (isNothing, isJust, fromJust)

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

  defaultMain tests' `catch` 
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

tests' :: TestTree
tests' = testGroup "Large Order Book Test" 
          [ testLargeOrderBookSell ]

-- ---------------------------------------------------------------------- 
-- Larger Tests
-- ---------------------------------------------------------------------- 

mockOrderBook :: OrderBook 
mockOrderBook = 
  -- BUY limit orders
  let lb1 = mkLimitOrder "pkhb1" 3 90 Buy    -- BUY 7xAsset @ $0.90
      lb2 = mkLimitOrder "pkhb2" 2 90 Buy    -- BUY 2xAsset @ $0.90
      lb3 = mkLimitOrder "pkhb3" 5 80 Buy    -- BUY 5xAsset @ $0.80
      lb4 = mkLimitOrder "pkhb4" 3 80 Buy    -- BUY 3xAsset @ $0.80
      lb5 = mkLimitOrder "pkhb5" 1 80 Buy    -- BUY 1xAsset @ $0.80
      lb6 = mkLimitOrder "pkhb6" 7 70 Buy    -- BUY 7xAsset @ $0.70
      lb7 = mkLimitOrder "pkhb7" 8 70 Buy    -- BUY 8xAsset @ $0.70
      lb8 = mkLimitOrder "pkhb8" 4 70 Buy    -- BUY 4xAsset @ $0.70
      lb9 = mkLimitOrder "pkhb9" 9 60 Buy    -- BUY 9xAsset @ $0.60

  -- SELL limit orders
      ls1 = mkLimitOrder "pkhs1" 5 110 Sell  -- SELL 5xAsset @ $1.10
      ls2 = mkLimitOrder "pkhs2" 3 110 Sell  -- SELL 3xAsset @ $1.10
      ls3 = mkLimitOrder "pkhs3" 6 120 Sell  -- SELL 6xAsset @ $1.20
      ls4 = mkLimitOrder "pkhs4" 2 120 Sell  -- SELL 2xAsset @ $1.20
      ls5 = mkLimitOrder "pkhs5" 3 120 Sell  -- SELL 3xAsset @ $1.20
      ls6 = mkLimitOrder "pkhs6" 7 130 Sell  -- SELL 7xAsset @ $1.30
      ls7 = mkLimitOrder "pkhs7" 9 130 Sell  -- SELL 9xAsset @ $1.30
      ls8 = mkLimitOrder "pkhs8" 1 130 Sell  -- SELL 1xAsset @ $1.30
      ls9 = mkLimitOrder "pkhs9" 2 140 Sell  -- SELL 1xAsset @ $1.40

      ob = addLimitOrders 
            [ lb1, lb2, lb3, lb4, lb5, lb6, lb7, lb8, lb9
            , ls1, ls2, ls3, ls4, ls5, ls6, ls7, ls8, ls9
            ] mkEmptyOrderBook
  in ob

testLargeOrderBookBuy :: TestTree 
testLargeOrderBookBuy = testCaseSteps "Test large order book" $ \step -> do
  
  let mob = mockOrderBook
      -- mo1 = mkMarketOrder "pkhm1" 1 Sell 
      (accAmt, accOs, ob) = takeOrdersForBuy (fromJust $ obCurAsk mob) 7 mob (0, [])

  step ("Order Book:\n" ++ show ob)
  step ("Order @ 110 Before:\n" ++ showOrdersAtLevel 110 mob)
  step ("Order @ 110 After:\n" ++ showOrdersAtLevel 110 ob)
  step ("AccOs:\n" ++ show accOs)
  step ("AccAmt:\n" ++ show accAmt)

testLargeOrderBookSell :: TestTree 
testLargeOrderBookSell = testCaseSteps "Test large order book" $ \step -> do
  
  let mob = mockOrderBook
      (accAmt, accOs, ob) = takeOrdersForSell (fromJust $ obCurBid mob) 6 mob (0, [])

  step ("Order Book:\n" ++ show ob)
  step ("Order @ 90 Before:\n" ++ showOrdersAtLevel 90 mob)
  step ("Order @ 80 Before:\n" ++ showOrdersAtLevel 80 mob)

  step ("Order @ 90 After:\n" ++ showOrdersAtLevel 90 ob)
  step ("Order @ 80 After:\n" ++ showOrdersAtLevel 80 ob)

  step ("AccOs:\n" ++ show accOs)
  step ("AccAmt:\n" ++ show accAmt)

-- ---------------------------------------------------------------------- 
-- Multi-Step Unit Tests
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
      -- BUY 10xAsset @ $0.90
      l1  = mkLimitOrder "pkh1" 10 90 Buy   
      ob2 = addLimitOrder l1 ob1

  step "Assert: Order book has 1 limit order"
  Map.size (obLimitOrders ob2) @?= 1

  step "Assert: Order book curBid is 90"
  obCurBid ob2 @?= Just 90

  let -- SELL 10xAsset @ $1.10
      l2  = mkLimitOrder "pkh2" 10 110 Sell 
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
        lb1 = mkLimitOrder "pkh1" 10 90  Buy   -- BUY 10xAsset @ $0.90
        ls1 = mkLimitOrder "pkh2" 20 110 Sell  -- SELL 20xAsset @ $1.10
        ob2 = addLimitOrders [lb1, ls1] ob1    -- Add limit orders

    step "Assert: Bid is set correctly (90)"
    obCurBid ob2 @?= Just 90

    step "Assert: Ask is set correctly (110)"
    obCurAsk ob2 @?= Just 110

    let lb2 = mkLimitOrder "pkh3" 5 80 Buy    -- BUY 5xAsset @ $0.80
        ls2 = mkLimitOrder "pkh4" 5 100 Sell  -- SELL 5xAsset Asset @ $1
        ob3 = addLimitOrders [lb2, ls2] ob2   -- Add limit orders

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
        lb1 = mkLimitOrder "pkh1" 1 80 Buy
        ls1 = mkLimitOrder "pkh2" 1 120 Sell
        ob2 = addLimitOrders [lb1, ls1] ob1

    obCurBid ob2 @?= Just 80
    obCurAsk ob2 @?= Just 120 

    let lb2 = mkLimitOrder "pkh3" 1 120 Buy  -- invalid
        ls2 = mkLimitOrder "pkh3" 1 70  Sell -- invalid
        ob3 = addLimitOrders [lb2, ls2] ob2

    step "Assert: Order book unchanged (invalid orders rejected)"
    ob3 @?= ob2

limitOrdersAtSamePriceLevel :: TestTree
limitOrdersAtSamePriceLevel = 
  testCaseSteps "Check adding limit orders at same price level" $ \step -> do 
    let ob1  = mkEmptyOrderBook
        lb1 = mkLimitOrder "pkh1" 1 80 Buy
        lb2 = mkLimitOrder "pkh2" 1 80 Buy
        ob2 = addLimitOrders [lb1, lb2] ob1

    step "Assert: Order book has 2 limit orders (2 BUY)"
    length (getOrdersAtKey 80 (obLimitOrders ob2)) @?= 2
    length (getFlattenedBuyOrders ob2)             @?= 2
    length (getFlattenedOrders ob2)                @?= 2

    let ls1 = mkLimitOrder "pkh3" 1 110 Sell  
        ls2 = mkLimitOrder "pkh4" 1 110 Sell  
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
        (accV, accOs, newOb) = takeOrdersForBuy curAsk 10 ob (0, []) 

    step "Assert: 10 consumed amount"
    accV @?= 10

    step "Assert: 1 consumed order"
    length accOs @?= 1

    step "Assert: Empty order book"
    Map.null (obLimitOrders newOb) @? "Order book not empty"

-- Test takeOrdersForSell
testTakeSellMarketOrder :: TestTree 
testTakeSellMarketOrder = 
  testCaseSteps "Simple SELL market order" $ \step -> do 
    let -- BUY $10 of Asset at $1.00 (value @10)
        lb1         = mkLimitOrder "pkh1" 10 100 Buy  
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
        mo  = mkMarketOrder "pkh2" 10 Buy 
        ob2 = executeMarketOrder mo ob1 

    step "Assert: Order book empty"
    null (getFlattenedOrders ob2) @? "Order book not empty"
 
testExecuteSellMarketOrder :: TestTree
testExecuteSellMarketOrder = 
  testCaseSteps "Simple execute SELL market order" $ \step -> do
    let -- BUY $10 of Asset at $1.00 (value 10xAsset)
        lb1 = mkLimitOrder "pkh1" 10 100 Buy 
        ob1 = addLimitOrder lb1 mkEmptyOrderBook 
    
    let -- SELL 10xAsset
        mo  = mkMarketOrder "pkh2" 10 Sell 
        ob2 = executeMarketOrder mo ob1 

    step "Assert: Order book empty"
    null (getFlattenedOrders ob2) @? "Order book not empty"

