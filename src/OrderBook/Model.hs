{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores  #-}

module OrderBook.Model where

import Data.Default
import Data.Map     (Map)
import Data.Map     qualified as Map
import Data.Maybe   (isJust, fromJust)
import Data.Monoid  (Sum(..))

-- ---------------------------------------------------------------------- 
-- Data types
-- ---------------------------------------------------------------------- 

type Value = Integer
type Price = Integer

data OrderBook = OrderBook
    { obLimitOrders  :: Map Value [Order]
   -- ^ A list of all limit orders in the order book
    , obMarketOrders :: [Order]
   -- ^ A list of all market orders in the order book
    , obLastPrice    :: !(Maybe Integer)
   -- ^ the last traded price
    , obCurBid       :: !(Maybe Integer)
   -- ^ current bid price, where traders are willing to buy
    , obCurAsk       :: !(Maybe Integer)
   -- ^ current ask price, where traders are willing to sell
    , obIncrement    :: !Value
    } deriving (Eq)

data Order = Order 
    { oType   :: !OrderType
   -- ^ whether the order is a market or limit order
    , oPkh    :: !String
   -- ^ public key hash of trader submitting this order
    , oAmount :: !Value
   -- ^ amount of the token or usd to trade
    , oSide   :: OrderSide
   -- ^ whether it's a buy or sell market order
    } deriving (Eq, Show)

data OrderType  
    = Market 
    | Limit  
        { otlMaxPrice :: Price
       -- ^ maximum price (the limit) to execute this order
        }
    deriving (Eq, Show)

data OrderSide 
    = Buy 
   -- ^ where max price is the highest price trader is willing to buy asset 
    | Sell
   -- ^ where max price is the lowest price trader is willing to sell asset
  deriving (Eq, Show)

-- ----------------------------------------------------------------------   
-- Instances
-- ----------------------------------------------------------------------   

instance Default OrderBook where 
  def = OrderBook 
    { obLimitOrders  = Map.empty
    , obMarketOrders = []
    , obLastPrice    = Nothing 
    , obCurBid       = Nothing
    , obCurAsk       = Nothing
    , obIncrement    = 10
    }

instance Show OrderBook where 
  show ob = 
           "Total orders:         " <> show (getTotalOrders ob)
      <> "\nTotal limit orders:   " <> show (getTotalLimitOrders ob)
      <> "\nTotal market orders:  " <> show (getTotalMarketOrders ob)
      <> "\nIncrement:            " <> show (obIncrement ob)
      <> "\nLast price:           " <> show (obLastPrice ob) 
      <> "\nBid price:            " <> show (obCurBid ob) 
      <> "\nAsk price:            " <> show (obCurAsk ob)
      <> "\nOrders:\n" <> levels
    where 
      levels = foldr (\(p', os) acc -> acc <> showPriceLevel p' os) 
                 mempty (Map.toList (obLimitOrders ob))

-- Render orders at a price level
showPriceLevel :: Value -> [Order] -> String 
showPriceLevel price os = 
  show price <> ": " <> show (length os) <> " orders.\n"

-- ----------------------------------------------------------------------   
-- Functions
-- ----------------------------------------------------------------------   

-- Instantiation

-- Create an empty order book
mkEmptyOrderBook :: OrderBook 
mkEmptyOrderBook = def

-- Create a market order
mkMarketOrder :: String -> Value -> OrderSide -> Order
mkMarketOrder = Order Market 

-- Create a limit order
mkLimitOrder :: String -> Value -> Price -> OrderSide -> Order
mkLimitOrder pkh val price = Order (Limit price) pkh val

-- Getting information

-- Return the spread of the order book as a bid and ask price
getSpread :: OrderBook -> Maybe Value 
getSpread ob = do  
    a' <- obCurAsk ob
    b' <- obCurBid ob
    return (a' - b')

-- Return the number of price levels with orders in the order book
getBookDepth :: OrderBook -> Integer 
getBookDepth ob = fromIntegral $ Map.size (obLimitOrders ob)

-- Return the total number of limit orders in an order book
getTotalLimitOrders :: OrderBook -> Integer
getTotalLimitOrders OrderBook{ obLimitOrders = os } = getSum $
  foldr (\(_, os') acc -> acc <> Sum (fromIntegral (length os'))) mempty (Map.toList os)

-- Return total number of market orders in an order book
getTotalMarketOrders :: OrderBook -> Integer 
getTotalMarketOrders OrderBook{ obMarketOrders = os } = fromIntegral $ length os

-- Return total number of orders in the order book
getTotalOrders :: OrderBook -> Integer
getTotalOrders ob = getTotalLimitOrders ob + getTotalMarketOrders ob

isMarketOrder :: Order -> Bool
isMarketOrder (Order Market _ _ _) = True 
isMarketOrder _ = False

-- Check if an order is a Buy
isBuyLimitOrder :: Order -> Bool 
isBuyLimitOrder (Order (Limit _) _ _ Buy) = True
isBuyLimitOrder _ = False

-- Manipulating

-- Add an order to an order book
addLimitOrder :: Order -> OrderBook -> OrderBook 
addLimitOrder o ob 
  | isMarketOrder o = ob 
  | otherwise = 
      let atPrice = otlMaxPrice (oType o)
          orders  = Map.insertWith (flip (++)) atPrice [o] (obLimitOrders ob) 
      in ob { obLimitOrders = orders }

-- Add multiple orders to an order book
addLimitOrders :: [Order] -> OrderBook -> OrderBook
addLimitOrders os ob = foldr addLimitOrder ob os 

-- Add market order to an order book
addMarketOrder :: Order -> OrderBook -> OrderBook 
addMarketOrder o ob
  | isMarketOrder o = let os = obMarketOrders ob in ob { obMarketOrders = o:os }
  | otherwise = ob

-- ----------------------------------------------------------------------   
-- Matching Engine Related
-- ----------------------------------------------------------------------   

-- Key will either by curBid or curAsk
getOrdersAtKey :: Value -> Map Value [Order] -> [Order]
getOrdersAtKey k m = m Map.! k

-- Remove orders from order book that will be consumed for a buy order, 
-- return updated orderbook
takeOrders :: OrderSide                   -- buy or sell
           -> Value                       -- key (initial value will be curBid or curAsk)
           -> Value                       -- target value to trade
           -> OrderBook                   -- order book to take orders from
           -> (Value, [Order])            -- accumulated value and orders to consume
           -> (Value, [Order], OrderBook) -- value + orders to consume, and updated order book
takeOrders side k tgt ob (curVal, os) = 
  let incFn       = if side == Buy then (+) else (-)  -- move key up or down the order book
      ordersAtKey = obLimitOrders ob Map.! k          -- orders to fold over

      (curVal', ordersToConsume, updatedOrderBook) = 
        foldr (\o (v', os', ob') -> 
          if v' >= tgt 
            -- target met, return value, orders to consume, and updated order book
            then (v', os', ob') 
            else 
              -- remove next order from order book
              let m' = Map.adjust (drop 1) k (obLimitOrders ob') 
              in (v' + oAmount o, os++[o], (ob' { obLimitOrders = m' })))
          (curVal, os, ob) ordersAtKey
      -- --------------------
      -- TODO: Handle lefover 
      -- --------------------
  in if curVal' > tgt 
        then (curVal', ordersToConsume, updatedOrderBook)
        else 
          let nextKey = k `incFn` obIncrement ob 
          in takeOrders side nextKey tgt updatedOrderBook (curVal', ordersToConsume)

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
      -- send value to the market order pkh and limit order pkhs.
      updatedOrderBook

-- Check if a market order is fillable
isMarketOrderFillable :: Order -> OrderBook -> Bool
isMarketOrderFillable o ob = getTotalLiquidityOnSide side ob >= oAmount o
  where
    -- Check liquidity on the OPPOSITE side of the market
    side = if oSide o == Buy then Sell else Buy

-- Return total liquidity on the buy or sell side of the market
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

-- Utilities to get a flattened order list of an order book
getFlattenedOrders :: OrderBook -> [Order]
getFlattenedOrders ob = concat $ snd <$> Map.toList (obLimitOrders ob)

getFlattenedBuyOrders :: OrderBook -> [Order]
getFlattenedBuyOrders ob = filter isBuyLimitOrder $ getFlattenedOrders ob

getFlattenedSellOrders :: OrderBook -> [Order]
getFlattenedSellOrders ob = filter (not . isBuyLimitOrder) $ getFlattenedOrders ob

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

