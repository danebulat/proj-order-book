{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores  #-}

module Main where

import OrderBook.Model
import OrderBook.Matching

-- ----------------------------------------------------------------------   
-- Main
-- ----------------------------------------------------------------------   

main :: IO ()
main = do 
  let -- A buy limit order to buy $10 of asset at $1
      l1 = mkLimitOrder "pkh1" 10_000 1_00 Buy 
      -- A buy limit order to sell $20 of asset at $1.10
      l2 = mkLimitOrder "pkh2" 20_000 1_10 Sell
      -- A market order to buy $5 of asset
      m1 = mkMarketOrder "pkh1" 5_00 Buy
      -- Add the limit and market orders to an empty order book
      ob = addMarketOrder m1 $ addLimitOrders [l1, l2] mkEmptyOrderBook

  print ob
  print $ isMarketOrderFillable m1 ob

