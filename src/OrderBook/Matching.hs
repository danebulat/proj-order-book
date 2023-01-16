{-# LANGUAGE ImportQualifiedPost #-}

module OrderBook.Matching where

import Data.Default
import Data.Map        (Map)
import Data.Map        qualified as Map
import Data.Maybe      (isJust, fromJust, fromMaybe)
import Data.Monoid     (Sum(..))
import OrderBook.Model

-- ----------------------------------------------------------------------   
-- Matching Engine Related
-- ----------------------------------------------------------------------   

-- TODO: 
-- Call `executeMarketOrder` for each market order in the order book

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

-- ----------------------------------------------------------------------  
-- Take (drop) orders from order book that will be consumed for 
-- an order and return the updated orderbook.
-- ----------------------------------------------------------------------  

updateAsk :: OrderBook -> Value -> OrderBook 
updateAsk ob curAsk = 
  let m = obLimitOrders ob
  in case m Map.! curAsk of 
    -- Empty price level key also deleted here
    [] -> case Map.lookupGT curAsk m of 
            Just (nextAsk, _) -> ob { obLimitOrders = Map.delete curAsk m
                                    , obCurAsk = Just nextAsk }
            Nothing           -> ob { obLimitOrders = Map.delete curAsk m
                                    , obCurAsk = Nothing      }
    _  -> ob

updateBid :: OrderBook -> Value -> OrderBook 
updateBid ob curBid = 
  let m = obLimitOrders ob
  in case m Map.! curBid of 
    -- Empty price level key also deleted here
    [] -> case Map.lookupLT curBid m of 
            Just (nextBid, _) -> ob { obLimitOrders = Map.delete curBid m
                                    , obCurBid = Just nextBid }
            Nothing           -> ob { obLimitOrders = Map.delete curBid m
                                    , obCurBid = Nothing      }
    _  -> ob

-- ---------------------------------------------------------------------- 
-- Construct a BUY market order
-- ---------------------------------------------------------------------- 

takeOrdersForBuy 
    :: Value                            -- key (initial value will be curBid or curAsk)
    -> Value                            -- target VALUE to trade (not target asset amount)
    -> OrderBook                        -- order book to take orders from
    -> (Value, [Order])                 -- accumulated value and orders to consume
    -> (Value, [Order], OrderBook)      -- value + orders to consume, and updated order book
takeOrdersForBuy k targetAmt ob (accV, accOs) =
  -- fold orders at this price level to match the market buy order
  let 
    (accV', accOs', updatedOrderBook) = 
     foldr (\o (v', os', ob') -> 
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
                      assetAmtLeft   = orderAssetAmt - leftoverAmt
                      orderToInsert  = o{ oAmount = assetAmtLeft }
                      orderValToTake = orderAssetAmt - leftoverAmt
                      m' = Map.adjust (\os -> orderToInsert:drop 1 os) k (obLimitOrders ob')
                    in (v'+orderValToTake, osAppended, (ob'{ obLimitOrders = m' }))
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

takeOrdersForSell 
    :: Value                            -- key (initial value will be curBid or curAsk)
    -> Value                            -- target VALUE to trade (not target asset amount)
    -> OrderBook                        -- order book to take orders from
    -> (Value, [Order])                 -- accumulated value and orders to consume
    -> (Value, [Order], OrderBook)      -- value + orders to consume, and updated order book
takeOrdersForSell k targetAmt ob (accAmt, accOs) =
  -- fold orders at this price level to match market sell order
  let 
    (accAmt', accOs', updatedOrderBook) = 
      foldr (\o (v', os', ob') -> 
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
                    -- keep order and set its amount to leftover value of asset
                    let 
                      assetAmtLeft   = orderAssetAmt - leftoverAmt 
                      orderToInsert  = o{ oAmount = assetAmtLeft }
                      orderAmtToTake = orderAssetAmt - leftoverAmt
                      m' = Map.adjust (\os -> orderToInsert:drop 1 os) k (obLimitOrders ob')
                    in (v'+orderAmtToTake, osAppended, (ob'{ obLimitOrders = m' }))
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
  | not (isValidLimitOrder o ob) = ob
  | otherwise = 
      let k = otlMaxPrice (oType o)
          m = Map.insertWith (flip (++)) k [o] (obLimitOrders ob) 
      in updateBidAsk (ob { obLimitOrders = m }) o

  -- Rejection criteria:
  -- 1. Limit price not divisible by order book increment 
  -- 2. BUY limit price  > current ask price
  -- 3. SELL limit price < current bid price
isValidLimitOrder :: Order -> OrderBook -> Bool 
isValidLimitOrder o ob 
  | side == Buy = 
      limitPrice `mod` inc == 0 &&
      (case obCurAsk ob of 
        Just p  -> limitPrice < p
        Nothing -> True)
  | otherwise = 
      limitPrice `mod` inc == 0 &&
      (case obCurBid ob of 
        Just p  -> limitPrice > p
        Nothing -> True)
  where 
    side = oSide o
    limitPrice = otlMaxPrice (oType o)
    inc = obIncrement ob

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
isMarketOrderFillable o ob = case oSide o of 
  Buy -> 
    let filteredOrders = getFlattenedSellOrders ob
        targetAmt = oAmount o
    in fst $ foldr (\o' (b, acc) -> 
                if b then (b, acc)
                else let amtInOrder = oAmount o' 
                     in (acc + amtInOrder >= targetAmt, acc + amtInOrder)) 
              (False, 0) filteredOrders 
  Sell -> 
    let filteredOrders = getFlattenedBuyOrders ob
        targetAmt = oAmount o
    in fst $ foldr (\o' (b, acc) -> 
                if b then (b, acc) 
                else let amtInOrder = oAmount o'
                     in (acc + amtInOrder >= targetAmt, acc + oAmount o'))
              (False, 0) filteredOrders

-- Return total liquidity on the buy or sell side of the market in usd
-- TODO: Cache total liquidity and update on executing limit orders
getTotalLiquidityOnSide :: OrderSide -> OrderBook -> Value
getTotalLiquidityOnSide side ob
  | side == Buy = foldVal (getFlattenedBuyOrders ob)
  | otherwise   = foldVal (getFlattenedSellOrders ob)
  where 
    foldVal os = foldr (\o acc -> let valInOrder = oAmount o * otlMaxPrice (oType o)
                                  in acc + valInOrder) 0 os

