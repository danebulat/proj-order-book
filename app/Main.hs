{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores  #-}

module Main where
import Data.Maybe (fromJust)
import OrderBook.Model
import OrderBook.Matching

-- ----------------------------------------------------------------------   
-- Main
-- ----------------------------------------------------------------------   

main :: IO ()
main = do 
  let -- Buy $10 of Asset @ $1.00 (value $10.00)
      l1 = mkLimitOrder "pkh1" 10_000 1_00 Buy 
      -- Sell 20xAsset @ $1.10 (value $22.00)
      l2 = mkLimitOrder "pkh2" 20 1_10 Sell
      ob = addLimitOrders [l1, l2] mkEmptyOrderBook
      
      -- A market order to buy $5 of asset
      m1 = mkMarketOrder "pkh3" 22_00 Buy

      (accV, accOs, ob1) = takeOrdersForBuy (fromJust $ obCurAsk ob) 22_00 ob (0,[]) 
      ob2 = executeMarketOrder m1 ob 

  -- print $ isMarketOrderFillable m1 ob
  print (ob1 == ob2)

