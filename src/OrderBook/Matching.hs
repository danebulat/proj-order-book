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

-- TODO: Call `executeMarketOrder` for each market order in the order 
--       book

-- Fill a market order and update the order book 
executeMarketOrder :: Order -> OrderBook -> OrderBook
executeMarketOrder o ob
  | not (isMarketOrderFillable o ob) = ob 
  | otherwise = case oSide o of 
      -- Handle buy market order
      Buy -> 
        let Just startKey = if oSide o == Buy then obCurAsk ob else obCurBid ob
            (valToConsume      -- usd to consume for buyer
             , ordersToConsume   -- use to build tx
             , updatedOrderBook  -- new state of order book
             ) = takeOrdersForBuy startKey (oAmount o) ob (0, [])
        in 
          -- At this point, we can construct the transaction that will 
          -- send value to the wallets involved in the tx.
          updatedOrderBook

      -- Handle sell market order
      Sell -> undefined

-- ----------------------------------------------------------------------  
-- Take (drop) orders from order book that will be consumed for 
-- an order and return the updated orderbook.
-- ----------------------------------------------------------------------  

--
-- Construct a BUY market order
--
takeOrdersForBuy 
    :: Value                            -- key (initial value will be curBid or curAsk)
    -> Value                            -- target VALUE to trade (not target asset amount)
    -> OrderBook                        -- order book to take orders from
    -> (Value, [Order])                 -- accumulated value and orders to consume
    -> (Value, [Order], OrderBook)      -- value + orders to consume, and updated order book
takeOrdersForBuy k target ob (accV, accOs)
  | null ordersAtKey = takeOrdersForBuy nextKey target ob (accV, accOs)
  | otherwise = 
  -- fold orders at this price level to match the market buy order
  let 
    (accV', accOs', updatedOrderBook) = 
     foldr (\o (v', os', ob') -> 
        if v' >= target then (v', os', ob')  -- target already met
          else 
            let orderAssetAmt      = oAmount o
                orderVal           = oAmount o * k
                curValPlusOrderVal = v' + orderVal
                osAppended         = os' ++ [o]
            in
            -- check target met when consuming this order
            if curValPlusOrderVal >= target then 
                -- leftover means a partially filled limit order
                let leftover = curValPlusOrderVal - target in 
                  if leftover > 0 then
                      -- keep order and set its amount to leftover value of asset
                      let 
                        assetAmtLeft   = orderAssetAmt - (leftover `div` k)
                        orderToInsert  = o{ oAmount = assetAmtLeft }
                        orderValToTake = orderVal - leftover
                        m' = Map.adjust (\os -> orderToInsert:drop 1 os) k (obLimitOrders ob')
                      in (v'+orderValToTake, osAppended, (ob'{ obLimitOrders = m' }))
                    else 
                      -- no leftover, remove entire order from order book
                      let m' = Map.adjust (drop 1) k (obLimitOrders ob')
                      in (curValPlusOrderVal, osAppended, ob'{ obLimitOrders = m' })
              -- target not yet met, take order
              else 
                let m' = Map.adjust (drop 1) k (obLimitOrders ob')
                in (curValPlusOrderVal, osAppended, ob'{ obLimitOrders = m' })) 
        (accV, accOs, ob) ordersAtKey
  in 
    if accV' > target 
      then (accV', accOs', updateBidAsk' updatedOrderBook Buy) -- ask updated
      else let obNewAsk = updatedOrderBook{ obCurAsk = Just nextKey }
           in takeOrdersForBuy nextKey target obNewAsk (accV', accOs')
  where 
    ordersAtKey = getOrdersAtKey k (obLimitOrders ob)
    nextKey = k + obIncrement ob

--
-- Constructing a SELL market order
--
takeOrdersForSell 
    :: Value                            -- key (initial value will be curBid or curAsk)
    -> Value                            -- target VALUE to trade (not target asset amount)
    -> OrderBook                        -- order book to take orders from
    -> (Value, [Order])                 -- accumulated value and orders to consume
    -> (Value, [Order], OrderBook)      -- value + orders to consume, and updated order book
takeOrdersForSell k targetAmt ob (accAmt, accOs) 
  | null ordersAtKey = takeOrdersForSell nextKey targetAmt ob (accAmt, accOs)
  | otherwise = 
  -- fold orders at this price level to match market sell order
  let 
    (accAmt', accOs', updatedOrderBook) = 
      foldr (\o (v', os', ob') -> 
        if v' >= targetAmt then (v', os', ob')  -- target already met
          else 
            let orderVal           = oAmount o 
                orderAssetAmt      = orderVal `div` k
                curAmtPlusOrderAmt = v' + orderVal
                osAppended         = os' ++ [o]
            in
            -- check target met when consuming this order
            if curAmtPlusOrderAmt >= targetAmt then 
                -- leftover means a partially filled limit order
                let leftoverAmt = curAmtPlusOrderAmt - targetAmt in 
                  if leftoverAmt > 0 then
                      -- keep order and set its amount to leftover value of asset
                      let 
                        assetAmtLeft   = orderAssetAmt - leftoverAmt 
                        orderToInsert  = o{ oAmount = assetAmtLeft }
                        orderAmtToTake = orderAssetAmt - leftoverAmt
                        m' = Map.adjust (\os -> orderToInsert:drop 1 os) k (obLimitOrders ob')
                      in (v'+orderAmtToTake, osAppended, (ob'{ obLimitOrders = m' }))
                    else 
                      -- no leftover, remove entire order from order book
                      let m' = Map.adjust (drop 1) k (obLimitOrders ob')
                      in (curAmtPlusOrderAmt, osAppended, ob'{ obLimitOrders = m' })
              -- target not yet met, take order
              else 
                let m' = Map.adjust (drop 1) k (obLimitOrders ob')
                in (curAmtPlusOrderAmt, osAppended, ob'{ obLimitOrders = m' })) 
        (accAmt, accOs, ob) ordersAtKey
  in 
    if accAmt' > targetAmt
      then (accAmt', accOs', updateBidAsk' updatedOrderBook Sell) -- bid updated
      else let obNewBid = updatedOrderBook{ obCurBid = Just nextKey }
           in takeOrdersForSell nextKey targetAmt obNewBid (accAmt', accOs') -- recurse
  where 
    ordersAtKey = getOrdersAtKey k (obLimitOrders ob)
    nextKey = k - obIncrement ob

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

-- Call after adding a new limit order
updateBidAsk :: OrderBook -> Order -> OrderBook
updateBidAsk ob o
  | s == Buy = case obCurBid ob of 
      Nothing -> ob{ obCurBid = Just p }
      Just p' -> if p' < p then ob{ obCurBid = Just p } else ob

  | otherwise = case obCurAsk ob of 
      Nothing -> ob{ obCurAsk = Just p }
      Just p' -> if p' > p then ob{ obCurAsk = Just p } else ob
    where 
      s = oSide o
      p = otlMaxPrice (oType o)

-- Call after executing a market order
updateBidAsk' :: OrderBook -> OrderSide -> OrderBook 
updateBidAsk' ob s 
  | s == Buy =
      -- check curAsk
      let Just k = obCurAsk ob 
      in if null $ obLimitOrders ob Map.! k
        then updateBidAsk' (ob{ obCurAsk = Just k }) s
        else ob{ obCurAsk = Just k }
  | otherwise = 
      -- check curBid
      let Just k = obCurBid ob
      in if null $ obLimitOrders ob Map.! k 
        then updateBidAsk' (ob{ obCurBid = Just k }) s
        else ob{ obCurBid = Just k }

-- ----------------------------------------------------------------------   
-- Utilities
-- ----------------------------------------------------------------------   

-- Key will either by curBid or curAsk
getOrdersAtKey :: Value -> Map Value [Order] -> [Order]
getOrdersAtKey k m
  | Map.member k m = m Map.! k
  | otherwise = []

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

