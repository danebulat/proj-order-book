{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores  #-}

module OrderBook.Model where

import Data.Default
import Data.Map     (Map)
import Data.Map     qualified as Map
import Data.Maybe   (isJust, fromJust)

-- ---------------------------------------------------------------------- 
-- Data types
-- ---------------------------------------------------------------------- 

type Value = Integer
type Price = Integer

data OrderBook = OrderBook
    { obOrders    :: Map Value [Order]
   -- ^ A list of all orders in the order book
    , obLastPrice :: !(Maybe Integer)
   -- ^ The last traded price
    , obCurBid    :: !(Maybe Integer)
   -- ^ current bid price, where traders are willing to buy
    , obCurAsk    :: !(Maybe Integer)
   -- ^ current ask price, where traders are willing to sell
    , obIncrement :: !Value
    } 

data Order = Order 
    { oType   :: !OrderType
   -- ^ whether the order is a market or limit order
    , oPkh    :: !String
   -- ^ public key hash of trader submitting this order
    , oAmount :: !Value
   -- ^ amount of the token or usd to trade
    } deriving Show

data OrderType  
    = Market 
  -- ^ market order which will execute immediately
    | Limit  
        { otlMaxPrice :: Price
       -- ^ maximum price (the limit) to execute this order
        , otlDir   :: LimitOrderType 
       -- ^ whether it's a buy limit or sell limit order
        }
    deriving Show

data LimitOrderType 
    = Buy 
   -- ^ where max price is the highest price trader is willing to buy asset 
    | Sell
   -- ^ where max price is the lowest price trader is willing to sell asset
  deriving Show

-- ----------------------------------------------------------------------   
-- Instances
-- ----------------------------------------------------------------------   

instance Default OrderBook where 
  def = OrderBook 
    { obOrders    = Map.empty
    , obLastPrice = Nothing 
    , obCurBid    = Nothing
    , obCurAsk    = Nothing
    , obIncrement = 10
    }

instance Show OrderBook where 
  show ob = 
           "Total orders:  " <> show (getTotalOrders ob)
      <> "\nIncrement:     " <> show (obIncrement ob)
      <> "\nLast price:    " <> show (obLastPrice ob) 
      <> "\nBid price:     " <> show (obCurBid ob) 
      <> "\nAsk price:     " <> show (obCurAsk ob)
      <> "\nOrders:\n" <> levels
    where 
      levels = foldr (\(p', os) acc -> acc <> showPriceLevel p' os) 
                 mempty (Map.toList (obOrders ob))

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
mkMarketOrder :: String -> Value -> Order
mkMarketOrder = Order Market 

-- Create a limit order
mkLimitOrder :: String -> Value -> Price -> LimitOrderType -> Order
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
getBookDepth ob = fromIntegral $ Map.size (obOrders ob)

getTotalOrders :: OrderBook -> Integer
getTotalOrders OrderBook{ obOrders = os } = 
  foldr (\(_, os') acc -> acc + fromIntegral (length os')) 0 (Map.toList os)

-- Manipulating

-- Add an order to an order book
addLimitOrder :: Order -> OrderBook -> OrderBook 
addLimitOrder o ob = ob { obOrders = orders }
  where 
    atPrice = otlMaxPrice (oType o)
    orders = Map.insertWith (flip (++)) atPrice [o] (obOrders ob) 

-- Add multiple orders to an order book
addLimitOrders :: [Order] -> OrderBook -> OrderBook
addLimitOrders os ob = foldr addLimitOrder ob os 

-- ----------------------------------------------------------------------   
-- Main
-- ----------------------------------------------------------------------   

main :: IO ()
main = do 
  let -- A buy limit order to buy $10 of asset at $1
      l1 = mkLimitOrder "pkh1" 10_000 1_00 Buy 
      -- A buy limit order to sell $20 of asset at $1.10
      l2 = mkLimitOrder "pkh2" 20_000 1_10 Sell
      -- Add the two limit orders to an empty order book
      ob = addLimitOrders [l1, l2] mkEmptyOrderBook

  print ob
