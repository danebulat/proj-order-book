{-# LANGUAGE ImportQualifiedPost #-}

module OrderBook.Utils where

import Data.Default
import Data.Map               (Map)
import Data.Map               qualified as Map

import OrderBook.Model

-- ---------------------------------------------------------------------- 
-- Update bid/ask based on a price
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
    _anyOther -> ob

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
    _anyOther  -> ob

-- ---------------------------------------------------------------------- 
-- Making order books and orders
-- ---------------------------------------------------------------------- 

-- Create an empty order book
mkEmptyOrderBook :: OrderBook 
mkEmptyOrderBook = def

-- Create a market order
mkMarketOrder :: String -> Value -> OrderSide -> Order
mkMarketOrder = Order Market 

-- Create a limit order
mkLimitOrder :: String      -- Public payment key hash
             -> Value       -- Amount of asset to buy/sell
             -> Price       -- Price level to buy/sell
             -> OrderSide   -- Buy or Sell
             -> Order
mkLimitOrder pkh amt price = Order (Limit price) pkh amt

-- ---------------------------------------------------------------------- 
-- Adding limit orders to an order book
-- ---------------------------------------------------------------------- 

-- Add multiple orders to an order book
addLimitOrders :: [Order] -> OrderBook -> OrderBook
addLimitOrders os ob = foldr addLimitOrder ob os 

-- Add an order to an order book
addLimitOrder :: Order -> OrderBook -> OrderBook 
addLimitOrder o ob 
  | isMarketOrder o = ob 
  | not (isValidLimitOrder o ob) = ob
  | otherwise = 
      let k = otlMaxPrice (oType o)
          m = Map.insertWith (++) k [o] (obLimitOrders ob) 
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

-- ---------------------------------------------------------------------- 
-- Getting information from order book
-- ---------------------------------------------------------------------- 

getTotalBuyOrders :: OrderBook -> Int
getTotalBuyOrders = length . getFlattenedBuyOrders

getTotalSellOrders :: OrderBook -> Int
getTotalSellOrders = length . getFlattenedSellOrders

-- Utilities to get a flattened order list of an order book
getFlattenedOrders :: OrderBook -> [Order]
getFlattenedOrders ob = concat $ snd <$> Map.toList (obLimitOrders ob)

getFlattenedBuyOrders :: OrderBook -> [Order]
getFlattenedBuyOrders ob = filter isBuyLimitOrder $ getFlattenedOrders ob

getFlattenedSellOrders :: OrderBook -> [Order]
getFlattenedSellOrders ob = filter (not . isBuyLimitOrder) $ getFlattenedOrders ob

-- Return the spread of the order book as a bid and ask price
getSpread :: OrderBook -> Maybe Value 
getSpread ob = do  
    a' <- obCurAsk ob
    b' <- obCurBid ob
    return (a' - b')

-- Return the number of price levels with orders in the order book
getBookDepth :: OrderBook -> Integer 
getBookDepth ob = fromIntegral $ Map.size (obLimitOrders ob)

-- Key will either by curBid or curAsk
getOrdersAtKey :: Value -> Map Value [Order] -> [Order]
getOrdersAtKey k m
  | Map.member k m = m Map.! k
  | otherwise = []

-- Return total liquidity on the buy or sell side of the market in usd
-- TODO: Cache total liquidity and update on executing limit orders
getTotalLiquidityOnSide :: OrderSide -> OrderBook -> Value
getTotalLiquidityOnSide side ob
  | side == Buy = foldVal (getFlattenedBuyOrders ob)
  | otherwise   = foldVal (getFlattenedSellOrders ob)
  where 
    foldVal os = foldr (\o acc -> let valInOrder = oAmount o * otlMaxPrice (oType o)
                                  in acc + valInOrder) 0 os

-- ---------------------------------------------------------------------- 
-- Querying order book functions
-- ---------------------------------------------------------------------- 

isMarketOrder :: Order -> Bool
isMarketOrder (Order Market _ _ _) = True 
isMarketOrder _ = False

-- Check if an order is a Buy
isBuyLimitOrder :: Order -> Bool 
isBuyLimitOrder (Order (Limit _) _ _ Buy) = True
isBuyLimitOrder _ = False

-- Check if a market order is fillable
isMarketOrderFillable :: Order -> OrderBook -> Bool
isMarketOrderFillable o ob = 
    fst $ foldr (\o' (b, acc) -> if b then (b, acc) 
              else let amtInOrder = oAmount o'
                   in (acc + amtInOrder >= targetAmt, acc + oAmount o'))
            (False, 0) flattenedOrders 
  where
    targetAmt = oAmount o
    flattenedOrders = case oSide o of 
      Buy  -> getFlattenedSellOrders ob
      Sell -> getFlattenedBuyOrders ob

-- ----------------------------------------------------------------------   
-- Rejection criteria:
-- 1. Limit price not divisible by order book increment 
-- 2. BUY limit price  > current ask price
-- 3. SELL limit price < current bid price
-- ----------------------------------------------------------------------   

-- Validate a limit order
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

