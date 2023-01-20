{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE DeriveAnyClass                #-}
{-# LANGUAGE DeriveGeneric                 #-}
{-# LANGUAGE DerivingStrategies            #-}
{-# LANGUAGE FlexibleContexts              #-}
{-# LANGUAGE TemplateHaskell #-}

module OrderBook.Model where

import Data.Aeson                     (FromJSON, ToJSON)
import GHC.Generics                   (Generic)

import Data.Default
import Data.Map               (Map)
import Data.Map               qualified as Map
import GHC.List               (foldl')

import Ledger                 qualified as L
import Plutus.V1.Ledger.Value qualified as V
import PlutusTx.Prelude       qualified as PP
import PlutusTx               qualified

-- ---------------------------------------------------------------------- 
-- Data types
-- ---------------------------------------------------------------------- 

type Price = Integer

data OrderBook = OrderBook
    { obLimitOrders  :: Map Price [Order]
   -- ^ A list of all limit orders in the order book
    , obCurBid       :: !(Maybe Integer)
   -- ^ current bid price, where traders are willing to buy
    , obCurAsk       :: !(Maybe Integer)
   -- ^ current ask price, where traders are willing to sell
    , obIncrement    :: !Integer
    }
    deriving stock (Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data Order = Order 
    { oType   :: !OrderType
   -- ^ whether the order is a market or limit order
    , oPkh    :: !L.PaymentPubKeyHash
   -- ^ public key hash of trader submitting this order
    , oAmount :: !Integer
   -- ^ amount of the token or usd to trade
    , oSide   :: OrderSide
   -- ^ whether it's a buy or sell market order
    } 
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data OrderType  
    = Market 
    | Limit  
        { otlMaxPrice :: Price
       -- ^ maximum price (the limit) to execute this order
        , otlNft :: V.AssetClass
       -- ^ nft attached to this order on-chain
        }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data OrderSide 
    = Buy 
   -- ^ where max price is the highest price trader is willing to buy asset 
    | Sell
   -- ^ where max price is the lowest price trader is willing to sell asset
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance PP.Eq OrderSide where 
  Buy  == Buy  = True
  Sell == Sell = True 
  _    == _    = False

PlutusTx.makeIsDataIndexed ''OrderSide [('Buy, 0), ('Sell, 1)]

-- ----------------------------------------------------------------------   
-- Default instance 
-- ----------------------------------------------------------------------   

instance Default OrderBook where 
  def = OrderBook 
    { obLimitOrders  = Map.empty
    , obCurBid       = Nothing
    , obCurAsk       = Nothing
    , obIncrement    = 10
    }

-- ----------------------------------------------------------------------   
-- Show Instance
-- ----------------------------------------------------------------------   

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

-- ----------------------------------------------------------------------   
-- Show functions
-- ----------------------------------------------------------------------   

-- Render orders at a price level
showPriceLevel :: Price -> [Order] -> String 
showPriceLevel price os = "    " <> 
  show price <> ": " <> show (length os) <> " orders\n"

showOrdersAtLevel :: Price -> OrderBook -> String
showOrdersAtLevel level ob = case mos of 
    Nothing  -> ""
    Just os' -> foldl' (\acc o -> acc <> showOrder o) mempty os' 
  where 
    mos = Map.lookup level (obLimitOrders ob)

showOrder :: Order -> String
showOrder o = show (oPkh o) 
           <> "  |  " <> show (oAmount o)
           <> "  |  " <> show (oSide o)
           <> "\n"

showOrders :: [Order] -> String
showOrders = foldl' (\acc o -> acc <> showOrder o) mempty 

getTotalLimitOrders :: OrderBook -> Int
getTotalLimitOrders OrderBook{ obLimitOrders = os } 
  | Map.null os = 0
  | otherwise = 
      foldr (\(_, os') acc -> 
          if not (null os') 
            then acc + length os'
            else acc) 
        0 (Map.toList os)
