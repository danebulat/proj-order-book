{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores  #-}

module Spec.LargeOrderBookTests where

import Data.Maybe (fromJust)
import Test.Tasty
import Test.Tasty.HUnit

import OrderBook.Model
import OrderBook.Matching
import OrderBook.Utils

tests :: TestTree
tests = testGroup "Large Order Book Tests"
          [ testExecuteBuyMarketOrder
          , testExecuteSellMarketOrder
          , testTakeOrdersForBuy
          , testTakeOrdersForSell
          ]

-- ---------------------------------------------------------------------- 
-- Mock Order Book
-- ---------------------------------------------------------------------- 

mockOrderBook :: OrderBook 
mockOrderBook = 
  -- BUY limit orders (total liquidity: 42xAsset)
  let lb1 = mkLimitOrder "pkhb1" 3 90 Buy    -- BUY 7xAsset @ $0.90
      lb2 = mkLimitOrder "pkhb2" 2 90 Buy    -- BUY 2xAsset @ $0.90
      lb3 = mkLimitOrder "pkhb3" 5 80 Buy    -- BUY 5xAsset @ $0.80
      lb4 = mkLimitOrder "pkhb4" 3 80 Buy    -- BUY 3xAsset @ $0.80
      lb5 = mkLimitOrder "pkhb5" 1 80 Buy    -- BUY 1xAsset @ $0.80
      lb6 = mkLimitOrder "pkhb6" 7 70 Buy    -- BUY 7xAsset @ $0.70
      lb7 = mkLimitOrder "pkhb7" 8 70 Buy    -- BUY 8xAsset @ $0.70
      lb8 = mkLimitOrder "pkhb8" 4 70 Buy    -- BUY 4xAsset @ $0.70
      lb9 = mkLimitOrder "pkhb9" 9 60 Buy    -- BUY 9xAsset @ $0.60

  -- SELL limit orders (total liquidity: 38)
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

-- ---------------------------------------------------------------------- 
-- Test: Execute Buy Market Order
-- ---------------------------------------------------------------------- 

testExecuteBuyMarketOrder :: TestTree 
testExecuteBuyMarketOrder = 
  testCaseSteps "Test large order book market buy" $ \step -> do
    let mo1  = mkMarketOrder "pkhm1" 10 Buy
        mob2 = executeMarketOrder mo1 mockOrderBook

    -- Assert: curAsk = 120, curBid = 90
    obCurAsk mob2 @?= Just 120
    obCurBid mob2 @?= Just 90

    -- Assert: total sell orders = 7, total buy orders = 9
    getTotalBuyOrders   mob2 @?= 9
    getTotalSellOrders  mob2 @?= 7
    getTotalLimitOrders mob2 @?= 16

    -- Show final order book
    step ("FINAL ORDER BOOK:\n" ++ show mob2)
 
-- ---------------------------------------------------------------------- 
-- Test: Execute Sell Market Order
-- ---------------------------------------------------------------------- 

testExecuteSellMarketOrder :: TestTree 
testExecuteSellMarketOrder = 
  testCaseSteps "Test large order book market sell" $ \step -> do
    let mo1  = mkMarketOrder "pkhm1" 10 Sell 
        mob2 = executeMarketOrder mo1 mockOrderBook

    -- Assert: curAsk = 110, curBid = 80
    obCurAsk mob2 @?= Just 110
    obCurBid mob2 @?= Just 80

    -- Assert: total sell orders = 9, total buy orders = 6
    getTotalBuyOrders   mob2 @?= 6
    getTotalSellOrders  mob2 @?= 9
    getTotalLimitOrders mob2 @?= 15

    -- Show final order book
    step ("FINAL ORDER BOOK:\n" ++ show mob2)

-- ---------------------------------------------------------------------- 
-- Test: Take Orders for Buy
-- ---------------------------------------------------------------------- 

testTakeOrdersForBuy :: TestTree
testTakeOrdersForBuy = 
  testCaseSteps "Test take orders for buy" $ \step -> do
    let mob         = mockOrderBook
        Just curAsk = obCurAsk mob

        -- Market order: Buy 20xAsset at curAsk ($1.10)
        (accAmt, accOs, newOb) = takeOrdersForBuy curAsk 20 mob (0,[])

    step "Asserting correct new order book"
    obCurAsk newOb @?= Just 130 
    obCurBid newOb @?= Just 90

    step "Asserting correct returned data"
    accAmt         @?= 20
    length accOs   @?= 6

    -- Show final order book and taken orders
    step ("FINAL ORDER BOOK:\n" ++ show newOb)
    step ("ORDERS TO CONSUME:\n" ++ renderOrders accOs)

-- ---------------------------------------------------------------------- 
-- Test: Take Orders for Sell 
-- ---------------------------------------------------------------------- 

testTakeOrdersForSell :: TestTree
testTakeOrdersForSell = 
  testCaseSteps "Test take orders for sell" $ \step -> do 
    let mob         = mockOrderBook 
        Just curBid = obCurBid mob
        
        -- Market order: Sell 20XAsset at curBid ($0.90)
        (accAmt, accOs, newOb) = takeOrdersForSell curBid 20 mob (0,[])

    step "Asserting correct new order book"
    obCurAsk newOb @?= Just 110
    obCurBid newOb @?= Just 70 

    step "Asserting correct returned data"
    accAmt       @?= 20
    length accOs @?= 6

    step ("FINAL ORDER BOOK:\n" ++ show newOb)
    step ("ORDERS TO CONSUME:\n" ++ renderOrders accOs)

-- ---------------------------------------------------------------------- 
-- Test: Ref 
-- ---------------------------------------------------------------------- 

testLargeOrderBook :: TestTree 
testLargeOrderBook = 
  testCaseSteps "Test large order book" $ \step -> do
    let mob = mockOrderBook
        (accAmt, accOs, ob) = takeOrdersForSell (fromJust $ obCurBid mob) 6 mob (0, [])

    step ("Order Book:\n" ++ show ob)
    step ("Order @ 90 Before:\n" ++ showOrdersAtLevel 90 mob)
    step ("Order @ 80 Before:\n" ++ showOrdersAtLevel 80 mob)

    step ("Orders @ 90 After:\n" ++ showOrdersAtLevel 90 ob)
    step ("Orders @ 80 After:\n" ++ showOrdersAtLevel 80 ob)

    step ("AccOs:\n" ++ renderOrders accOs)
    step ("AccAmt:\n" ++ show accAmt)


