{-# LANGUAGE AllowAmbiguousTypes           #-}
{-# LANGUAGE DataKinds                     #-}
{-# LANGUAGE DeriveAnyClass                #-}
{-# LANGUAGE DeriveGeneric                 #-}
{-# LANGUAGE DerivingStrategies            #-}
{-# LANGUAGE FlexibleContexts              #-}
{-# LANGUAGE FlexibleInstances             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving    #-}
{-# LANGUAGE ImportQualifiedPost           #-}
{-# LANGUAGE MultiParamTypeClasses         #-}
{-# LANGUAGE NoImplicitPrelude             #-}
{-# LANGUAGE PartialTypeSignatures         #-}
{-# LANGUAGE RecordWildCards               #-}
{-# LANGUAGE ScopedTypeVariables           #-}
{-# LANGUAGE TemplateHaskell               #-}
{-# LANGUAGE TypeApplications              #-}
{-# LANGUAGE TypeFamilies                  #-}
{-# LANGUAGE TypeOperators                 #-}
{-# LANGUAGE OverloadedStrings             #-}

{-# OPTIONS_GHC -fno-warn-unused-imports   #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module OnChain where

import Data.Aeson                (FromJSON, ToJSON)
import Control.Lens              ((^.))
import GHC.Generics              (Generic)
import PlutusTx                  qualified
import PlutusTx.Prelude          hiding (pure, (<$>))
import Plutus.V2.Ledger.Api      qualified as LV2
import Plutus.V2.Ledger.Contexts qualified as LV2C
import Plutus.V2.Ledger.Tx       qualified as LTXV2
import Plutus.V1.Ledger.Value    qualified as V
import Prelude                   qualified as P

import Plutus.Script.Utils.V2.Typed.Scripts.Validators qualified as V2UtilsTypeScripts
import Plutus.Script.Utils.V2.Typed.Scripts            qualified as Scripts
import Plutus.Script.Utils.V2.Scripts                  (scriptCurrencySymbol)

import Ledger                    qualified as L
import Ledger.Ada                qualified as Ada
import Ledger.Address            qualified as LV1Address
import Playground.Contract       (ToSchema)

-- ---------------------------------------------------------------------- 
-- Parameter
-- ---------------------------------------------------------------------- 

data Param = Param 
    { matcherPkh :: L.PaymentPubKeyHash
  -- ^ pkh of matcher "manager" wallet
    , assetA     :: L.AssetClass 
  -- ^ first asset in trading pair
    , assetB     :: L.AssetClass 
  -- ^ second asset in trading pair
    }
    deriving (P.Show, Generic)
    deriving anyclass (ToJSON, FromJSON, ToSchema)

PlutusTx.makeIsDataIndexed ''Param [('Param, 0)]
PlutusTx.makeLift ''Param

-- ---------------------------------------------------------------------- 
-- Datum
-- ---------------------------------------------------------------------- 

-- TODO: Import order book types
data OrderSide = Buy | Sell
    deriving (P.Show, Eq)

data Dat = Dat
    { traderPkh :: L.PaymentPubKeyHash
   -- ^ pkh of wallet adding liquidity
    , amount    :: V.Value 
   -- ^ amount of liquidity being added to order book
    , side      :: OrderSide
   -- ^ whether order is a buy or sell 
    , atPrice   :: Integer 
   -- ^ price trader wishes to buy assetA / sell assetB
    }

PlutusTx.makeIsDataIndexed ''OrderSide [('Buy, 0), ('Sell, 1)]
PlutusTx.makeIsDataIndexed ''Dat [('Dat, 0)]

-- ---------------------------------------------------------------------- 
-- Redeemer
-- ---------------------------------------------------------------------- 

data Redeem 
    = Spend       -- spend utxo to execute a trade
        Integer   -- amount of asset to buy/sell in market order
    | Cancel      -- cancel this order and refund trader

PlutusTx.makeIsDataIndexed ''Redeem [('Spend, 0), ('Cancel, 1)]

-- ---------------------------------------------------------------------- 
-- Validator types
-- ---------------------------------------------------------------------- 

data Trade
instance Scripts.ValidatorTypes Trade where
  type instance RedeemerType Trade = Redeem
  type instance DatumType    Trade = Dat

-- ---------------------------------------------------------------------- 
-- Validator script
-- ---------------------------------------------------------------------- 

{-# INLINEABLE mkValidator #-}
mkValidator :: Param -> Dat -> Redeem -> LV2.ScriptContext -> Bool 
mkValidator p dat red ctx = case red of 
    Spend amountToTrade ->
      case side dat of 
        
        -- This UTXO has deposited USD token (asset 2) and wants to 
        -- trade ADA (asset 1)
        Sell ->
          allInputsOnSide Buy   -- Tx is only consuming Buy UTXOs 
                                -- Tx total input value
                                -- Tx total output value (sent to traderPkh + own pkh only)
                                -- Tx input has exactly enough funds to cover trade
                                -- Partially filled orders produce an output with remaining value

        -- This UTXO has deposited ADA token (asset 1) and wants to 
        -- trade USD (asset 2)
        Buy ->
          allInputsOnSide Sell  -- Tx is only consuming Sell UTXOs 
                                -- Tx total input value 
                                -- Tx total output value (sent to traderPkh + own pkh only)
                                -- Tx input has exactly enough funds to cover trade
                                -- Partially filled orders produce an output with remaining value

    -- Trader wishes to cancel this order and get back their deposit
    Cancel -> 
      signedByTrader && fullValueReturned
  
  where 
    txInfo :: LV2C.TxInfo 
    txInfo = LV2C.scriptContextTxInfo ctx

    ownInput :: LV2.TxOut 
    ownInput = case LV2C.findOwnInput ctx of 
      Just txInInfo -> LV2.txInInfoResolved txInInfo
      Nothing -> traceError "input missing"

    signedByTrader :: Bool 
    signedByTrader =traceIfFalse "Wrong signer" $
      LV2C.txSignedBy txInfo $ L.unPaymentPubKeyHash (traderPkh dat)

    fullValueReturned :: Bool 
    fullValueReturned = LV2C.valuePaidTo txInfo pkh ==  ownInput ^. LTXV2.outValue
      where pkh = L.unPaymentPubKeyHash (traderPkh dat)

    -- ------------------------------------------------------------ 

    getInputs :: [LV2C.TxInInfo]
    getInputs = LV2C.txInfoInputs txInfo 

    allInputsOnSide :: OrderSide -> Bool 
    allInputsOnSide targetSide = traceIfFalse "Input on wrong side" $
      foldl (\b txInInfo -> if b then b else
              let o = LV2.txInInfoResolved txInInfo 
              in side (getDatum o) == targetSide ) False getInputs
     
-- ---------------------------------------------------------------------- 
-- Utilities
-- ---------------------------------------------------------------------- 

{-# INLINABLE minLovelace #-}
minLovelace :: V.Value
minLovelace = Ada.lovelaceValueOf (Ada.getLovelace  L.minAdaTxOut)

{-# INLINABLE getDatum #-}
getDatum :: LV2.TxOut -> Dat
getDatum txOut = case txOut ^. LTXV2.outDatum of 
  LTXV2.NoOutputDatum     -> traceError "Datum error"
  LTXV2.OutputDatumHash _ -> traceError "Datum error"
  LTXV2.OutputDatum d     -> case PlutusTx.fromBuiltinData $ LV2.getDatum d of 
                               Nothing -> traceError "Datum error"
                               Just d' -> d'
  
