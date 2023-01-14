{-# LANGUAGE ImportQualifiedPost #-}

module OrderBook.Matching where

import Data.Default
import Data.Map        (Map)
import Data.Map        qualified as Map
import Data.Maybe      (isJust, fromJust)
import Data.Monoid     (Sum(..))
import OrderBook.Model

-- ----------------------------------------------------------------------   
-- Matching Engine Related
-- ----------------------------------------------------------------------   

-- Remove orders from order book that will be consumed for an order, 
-- Return the updated orderbook and orders taken from it.
takeOrders 
    :: OrderSide                                -- buy or sell
    -> Value                                    -- key (initial value will be curBid or curAsk)
    -> Value                                    -- target value to trade
    -> OrderBook                                -- order book to take orders from
    -> (Value, [Order])                         -- accumulated value and orders to consume
    -> (Value, [Order], OrderBook)              -- value + orders to consume, and updated order book
takeOrders side k tgt ob (curVal, os) = 
  let incFn = if side == Buy then (+) else (-)          -- move key up or down the order book
      ordersAtKey = getOrdersAtKey k (obLimitOrders ob) -- orders to fold over

      -- fold orders at this price level to match market order
      (curVal', ordersToConsume, updatedOrderBook) = 
        foldr (\o (v', os', ob') -> 
          if v' >= tgt 
            then
              -- target already met
              (v', os', ob')
            else 
              -- check target met when consuming this order
              if v' + oAmount o >= tgt 
                then 
                  -- leftover means an partially filled limit order
                  let leftover = (v' + oAmount o) - tgt 
                  in 
                    if leftover > 0 
                      then
                        -- keep order and set amount to leftover value
                        let orderToInsert = o{ oAmount = leftover }
                            m' =  Map.adjust (\os -> orderToInsert : drop 1 os) k (obLimitOrders ob')
                        in (v' + oAmount o, os++[o], (ob'{ obLimitOrders = m' }))
                      else 
                        -- no leftover, remove entire order from order book
                        let m' = Map.adjust (drop 1) k (obLimitOrders ob')
                        in (v' + oAmount o, os++[o], ob'{ obLimitOrders = m' })
                else 
                  -- drop this order from the order book
                  let m' = Map.adjust (drop 1) k (obLimitOrders ob')
                  in (v' + oAmount o, os++[o], ob'{ obLimitOrders = m' })) 
          (curVal, os, ob) ordersAtKey
  in 
    if curVal' > tgt 
      then 
        (curVal', ordersToConsume, updatedOrderBook)
      else 
        -- Recurse
        let nextKey = k `incFn` obIncrement ob 
            updatedOrderBook' = 
              if side == Buy 
                then updatedOrderBook{ obCurAsk = Just nextKey }
                else updatedOrderBook{ obCurBid = Just nextKey }
        in takeOrders side nextKey tgt updatedOrderBook' (curVal', ordersToConsume)

-- Fill a market order and update the order book 
executeMarketOrder :: Order -> OrderBook -> OrderBook
executeMarketOrder o ob
  | not (isMarketOrderFillable o ob) = ob 
  | otherwise = 
    let Just startKey = if oSide o == Buy then obCurAsk ob else obCurBid ob
        ( valToConsume
          , ordersToConsume   -- use to build tx
          , updatedOrderBook  -- new state of order book
          ) = takeOrders (oSide o) startKey (oAmount o) ob (0, [])
    in 
      -- At this point, we can construct the transaction that will 
      -- send value to the wallets involved in the tx.
      
      updatedOrderBook

-- ----------------------------------------------------------------------   
-- Adding Orders 
-- ----------------------------------------------------------------------   

-- Add multiple orders to an order book
addLimitOrders :: [Order] -> OrderBook -> OrderBook
addLimitOrders os ob = foldr addLimitOrder ob os 

-- Add market order to an order book
addMarketOrder :: Order -> OrderBook -> OrderBook 
addMarketOrder o ob
  | isMarketOrder o = let os = obMarketOrders ob in ob { obMarketOrders = o:os }
  | otherwise = ob

-- Add an order to an order book
addLimitOrder :: Order -> OrderBook -> OrderBook 
addLimitOrder o ob 
  | isMarketOrder o = ob 
  | otherwise = 
      let k = otlMaxPrice (oType o)
          m = Map.insertWith (flip (++)) k [o] (obLimitOrders ob) 
      in updateBidAsk (ob { obLimitOrders = m }) o

-- Potentially update the bid/ask based on a new limit order
updateBidAsk :: OrderBook -> Order -> OrderBook
updateBidAsk ob o
  | s == Buy  = case obCurBid ob of 
      Nothing -> ob{ obCurBid = Just p }
      Just p' -> if p' < p then ob{ obCurBid = Just p } else ob

  | otherwise = case obCurAsk ob of 
      Nothing -> ob{ obCurAsk = Just p }
      Just p' -> if p' > p then ob{ obCurAsk = Just p } else ob
    where 
      s = oSide o
      p = otlMaxPrice (oType o)

-- ----------------------------------------------------------------------   
-- Utilities
-- ----------------------------------------------------------------------   

-- Key will either by curBid or curAsk
getOrdersAtKey :: Value -> Map Value [Order] -> [Order]
getOrdersAtKey k m = m Map.! k

-- Check if a market order is fillable
isMarketOrderFillable :: Order -> OrderBook -> Bool
isMarketOrderFillable o ob = getTotalLiquidityOnSide side ob >= oAmount o
  where side = if oSide o == Buy then Sell else Buy

-- Return total liquidity on the buy or sell side of the market
-- TODO: Cache total liquidity and update on executing limit orders
getTotalLiquidityOnSide :: OrderSide -> OrderBook -> Value
getTotalLiquidityOnSide side ob
  | side == Buy = 
      let filteredOrders = filter isBuyLimitOrder flattenedOrders
      in foldVal filteredOrders
  | otherwise = 
      let filteredOrders = filter (not . isBuyLimitOrder) flattenedOrders
      in foldVal filteredOrders
  where 
    flattenedOrders = concat $ snd <$> Map.toList (obLimitOrders ob)
    foldVal os = foldr (\o acc -> acc + oAmount o) 0 os

