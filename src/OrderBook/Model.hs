{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores  #-}

module OrderBook.Model where

import Data.Default
import Data.Map     (Map)
import Data.Map     qualified as Map
import Data.Maybe   (isJust, fromJust)
import Data.Monoid  (Sum(..))
import GHC.List     (foldl')

-- ---------------------------------------------------------------------- 
-- Data types
-- ---------------------------------------------------------------------- 

type Value = Integer
type Price = Integer

data OrderBook = OrderBook
    { obLimitOrders  :: Map Value [Order]
   -- ^ A list of all limit orders in the order book
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
    , obCurBid       = Nothing
    , obCurAsk       = Nothing
    , obIncrement    = 10
    }

instance Show OrderBook where 
  show ob = 
           "Total limit orders:   " <> show (getTotalLimitOrders ob)
      <> "\nIncrement:            " <> show (obIncrement ob)
      <> "\nBid price:            " <> show (obCurBid ob) 
      <> "\nAsk price:            " <> show (obCurAsk ob)
      <> "\nOrders:\n" <> levels
    where 
      levels = foldr (\(p', os) acc -> acc <> showPriceLevel p' os) 
                 mempty (Map.toList (obLimitOrders ob))

-- Render orders at a price level
showPriceLevel :: Value -> [Order] -> String 
showPriceLevel price os = "    " <> 
  show price <> ": " <> show (length os) <> " orders.\n"

showOrdersAtLevel :: Value -> OrderBook -> String
showOrdersAtLevel level ob = case mos of 
    Nothing  -> ""
    Just os' -> foldl' (\acc o -> acc <> renderOrder o) mempty os' 
  where 
    mos = Map.lookup level (obLimitOrders ob)

renderOrder :: Order -> String
renderOrder o = oPkh o <> "  |  " <> show (oAmount o)
                       <> "  |  " <> show (oSide o)
                       <> "\n"

renderOrders :: [Order] -> String
renderOrders = foldl' (\acc o -> acc <> renderOrder o) mempty 

-- ----------------------------------------------------------------------   
-- Utils
-- ----------------------------------------------------------------------   

-- Return the total number of limit orders in an order book
getTotalLimitOrders :: OrderBook -> Int
getTotalLimitOrders OrderBook{ obLimitOrders = os } 
  | Map.null os = 0
  | otherwise = 
      foldr (\(_, os') acc -> 
          if not (null os') 
            then acc + length os'
            else acc) 
        0 (Map.toList os)

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

isMarketOrder :: Order -> Bool
isMarketOrder (Order Market _ _ _) = True 
isMarketOrder _ = False

-- Check if an order is a Buy
isBuyLimitOrder :: Order -> Bool 
isBuyLimitOrder (Order (Limit _) _ _ Buy) = True
isBuyLimitOrder _ = False

-- ----------------------------------------------------------------------   
-- Instantiation
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
