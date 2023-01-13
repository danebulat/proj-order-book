{-# LANGUAGE ImportQualifiedPost #-}

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
    } deriving Show

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
-- Default Instance 
-- ----------------------------------------------------------------------   

instance Default OrderBook where 
  def = OrderBook 
    { obOrders    = Map.empty
    , obLastPrice = Nothing 
    , obCurBid    = Nothing
    , obCurAsk    = Nothing
    , obIncrement = 10
    }

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

-- TODO: getBookDepth
--   The number of price levels are available at any particular time 
--   in the book.

-- Manipulating

-- Add an order to an order book
addOrder :: Order -> Value -> OrderBook -> OrderBook 
addOrder o price ob = ob { obOrders = orders }
  where 
    orders = Map.insertWith (flip (++)) price [o] (obOrders ob) 

