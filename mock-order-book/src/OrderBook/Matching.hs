{-# LANGUAGE ImportQualifiedPost #-}

module OrderBook.Matching where

import Data.Default
import Data.Map        (Map)
import Data.Map        qualified as Map
import Data.Maybe      (isJust, fromJust, fromMaybe)
import Data.Monoid     (Sum(..))

import OrderBook.Model
import OrderBook.Utils

-- ----------------------------------------------------------------------   
-- Matching Engine Related
-- ----------------------------------------------------------------------   

-- Fill a market order and update the order book 
executeMarketOrder :: Order -> OrderBook -> OrderBook
executeMarketOrder o ob
  | not (isMarketOrderFillable o ob) = ob 
  | otherwise = case oSide o of 
      -- Handle buy market order
      Buy -> 
        let Just startKey = obCurAsk ob 
            (  valToConsume      -- usd to consume for buyer
             , ordersToConsume   -- use to build tx
             , updatedOrderBook  -- new state of order book
             ) = takeOrdersForBuy startKey (oAmount o) ob (0, [])
        in 
          -- At this point, we can construct the transaction that will 
          -- send value to the wallets involved in the tx.
          updatedOrderBook

      -- Handle sell market order
      Sell -> 
        let Just startKey = obCurBid ob 
            (  amtToConsume
             , ordersToConsume 
             , updatedOrderBook
             ) = takeOrdersForSell startKey (oAmount o) ob (0, [])
        in 
          -- At this point, we can construct the transaction that will 
          -- send value to the wallets involved in the tx.
          updatedOrderBook

-- ====================================================================== 
-- Take (drop) orders from order book that will be consumed for 
-- an order and return the updated orderbook.
-- ====================================================================== 

-- Parameters
--
-- Value                         Key (initial value will be curBid or curAsk)
-- Value                         Target amount to trade 
-- OrderBook                     Order book to take orders from
-- (Value, [Order])              Accumulated amount and orders to consume
-- (Value, [Order], OrderBook)   Asset amount + orders to consume + updated order book

-- ---------------------------------------------------------------------- 
-- Construct a BUY market order
-- ---------------------------------------------------------------------- 

takeOrdersForBuy :: Value -> Value -> OrderBook -> (Value, [Order]) 
                 -> (Value, [Order], OrderBook)
takeOrdersForBuy k targetAmt ob (accV, accOs) =
  -- fold orders at this price level to match the market buy order
  let 
    (accV', accOs', updatedOrderBook) = 
     foldl (\(v', os', ob') o -> 
        if v' >= targetAmt then (v', os', ob')  -- target already met
        else 
          let orderAssetAmt      = oAmount o
              curAmtPlusOrderAmt = v' + orderAssetAmt
              osAppended         = os' ++ [o]
          in
          -- check target met when consuming this order
          if curAmtPlusOrderAmt >= targetAmt then  
              -- leftover means a partially filled limit order
              let leftoverAmt = curAmtPlusOrderAmt - targetAmt in
                if leftoverAmt > 0 then
                    -- keep order and set its amount to leftover amount of asset
                    let 
                      orderAmtToTake = orderAssetAmt - leftoverAmt 
                      orderToInsert  = o{ oAmount = orderAssetAmt - orderAmtToTake } 
                      orderToTake    = o{ oAmount = orderAmtToTake }
                      m' = Map.adjust (\(_:os) -> orderToInsert : os) k (obLimitOrders ob')
                    in (v'+orderAmtToTake, os'++[orderToTake], (ob'{ obLimitOrders = m' }))
                  else 
                    -- no leftover, remove entire order from order book
                    let m' = Map.adjust (drop 1) k (obLimitOrders ob')
                    in (curAmtPlusOrderAmt, osAppended, ob'{ obLimitOrders = m' })
            -- target not yet met, take order
            else 
              let m' = Map.adjust (drop 1) k (obLimitOrders ob')
              in (curAmtPlusOrderAmt, osAppended, ob'{ obLimitOrders = m' })) 
        (accV, accOs, ob) ordersAtKey
  in 
    if accV' >= targetAmt 
      then 
        (accV', accOs', updateAsk updatedOrderBook k) 
      else 
        -- orders at this level all consumed, delete key and update ask
        let newOb   = updateAsk updatedOrderBook k
            nextKey = fromJust (obCurAsk newOb)
        in takeOrdersForBuy nextKey targetAmt newOb (accV', accOs')
  where 
    ordersAtKey = getOrdersAtKey k (obLimitOrders ob)

-- ---------------------------------------------------------------------- 
-- Constructing a SELL market order
-- ---------------------------------------------------------------------- 

takeOrdersForSell :: Value -> Value -> OrderBook -> (Value, [Order])
                  -> (Value, [Order], OrderBook)
takeOrdersForSell k targetAmt ob (accAmt, accOs) =
  -- fold orders at this price level to match market sell order
  let 
    (accAmt', accOs', updatedOrderBook) = 
      foldl (\(v', os', ob') o -> 
        if v' >= targetAmt then (v', os', ob') -- target already met
        else 
          let orderAssetAmt      = oAmount o 
              curAmtPlusOrderAmt = v' + orderAssetAmt
              osAppended         = os' ++ [o]
          in
          -- check target met when consuming this order
          if curAmtPlusOrderAmt >= targetAmt then 
              -- leftover means a partially filled limit order
              let leftoverAmt = curAmtPlusOrderAmt - targetAmt in 
                if leftoverAmt > 0 then
                    -- keep order and set its amount to leftover value of asset
                    let 
                      orderAmtToTake = orderAssetAmt - leftoverAmt
                      orderToInsert  = o{ oAmount = orderAssetAmt - orderAmtToTake } 
                      orderToTake    = o{ oAmount = orderAmtToTake }
                      m' = Map.adjust (\(_:os) -> orderToInsert : os) k (obLimitOrders ob')
                    in (v'+orderAmtToTake, os'++[orderToTake], (ob'{ obLimitOrders = m' }))
                  else 
                    -- no leftover, remove entire limit order from order book
                    let m' = Map.adjust (drop 1) k (obLimitOrders ob')
                    in (curAmtPlusOrderAmt, osAppended, ob'{ obLimitOrders = m' })
            -- target not yet met, take order
            else 
              let m' = Map.adjust (drop 1) k (obLimitOrders ob')
              in (curAmtPlusOrderAmt, osAppended, ob'{ obLimitOrders = m' })) 
        (accAmt, accOs, ob) ordersAtKey
  in 
    if accAmt' >= targetAmt
      then (accAmt', accOs', updateBid updatedOrderBook k)
      else 
        -- orders at this level all consumed, delete key and update bid 
        let newOb   = updateBid updatedOrderBook k 
            nextKey = fromJust (obCurBid newOb)
        in takeOrdersForSell nextKey targetAmt newOb (accAmt', accOs')
  where 
    ordersAtKey = getOrdersAtKey k (obLimitOrders ob)

