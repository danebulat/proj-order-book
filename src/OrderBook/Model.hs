
module OrderBook.Model where

-- ---------------------------------------------------------------------- 
-- Data types
-- ---------------------------------------------------------------------- 

type Value = Integer
type Price = Integer

data OrderBook = OrderBook
    { obOrders    :: [Order]
   -- ^ A list of all orders in the order book
    , obLastPrice :: !Integer 
   -- ^ The last traded price
    , obCurBid    :: !Integer
   -- ^ current bid price, where traders are willing to buy
    , obCurAsk    :: !Integer
   -- ^ current ask price, where traders are willing to sell
    } deriving Show

data Order = Order 
    { oPkh    :: !String
   -- ^ public key hash of trader submitting this order
    , oType   :: !OrderType
   -- ^ whether the order is a market or limit order
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
-- Functions
-- ----------------------------------------------------------------------   

emptyOrderBook :: OrderBook 
emptyOrderBook = OrderBook [] (-1) (-1) (-1)


