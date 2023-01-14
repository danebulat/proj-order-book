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
    } deriving (Eq, Show)

data OrderType  
    = Market 
        { moDirection :: OrderDirection 
       -- ^ whether it's a buy or sell market order
        }
    | Limit  
        { otlMaxPrice :: Price
       -- ^ maximum price (the limit) to execute this order
        , otlDir      :: OrderDirection 
       -- ^ whether it's a buy limit or sell limit order
        }
    deriving (Eq, Show)

data OrderDirection
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
mkMarketOrder :: String -> Value -> OrderDirection -> Order
mkMarketOrder pkh val dir = Order (Market dir) pkh val

-- Create a limit order
mkLimitOrder :: String -> Value -> Price -> OrderDirection -> Order
mkLimitOrder pkh val price loType = Order (Limit price loType) pkh val

-- Getting information

-- Return the spread if the order book as a bid and ask price
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
isMarketOrder (Order (Market _) _ _) = True 
isMarketOrder _ = False

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
-- Main
-- ----------------------------------------------------------------------   

main :: IO ()
main = do 
  let -- A buy limit order to buy $10 of asset at $1
      l1 = mkLimitOrder "pkh1" 10_000 1_00 Buy 
      -- A buy limit order to sell $20 of asset at $1.10
      l2 = mkLimitOrder "pkh2" 20_000 1_10 Sell
      -- A market order to buy $5 of asset
      m1 = mkMarketOrder "pk1" 5_00 Buy
      -- Add the limit and market orders to an empty order book
      ob = addMarketOrder m1 $ addLimitOrders [l1, l2] mkEmptyOrderBook
   
  print ob
