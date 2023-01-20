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
{-# LANGUAGE NumericUnderscores            #-}
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
import Playground.Contract       (ToSchema)
import OrderBook.Model           (OrderSide(..))

-- ---------------------------------------------------------------------- 
-- Parameter
-- ---------------------------------------------------------------------- 

data Param = Param 
    { assetA     :: L.AssetClass 
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

data Dat = Dat
    { traderPkh     :: L.PaymentPubKeyHash
   -- ^ pkh of wallet adding liquidity
    , datAmount     :: Integer
   -- ^ amount of asset to trade
    , side          :: OrderSide
   -- ^ whether order is a buy or sell 
    , tradePrice    :: Integer 
   -- ^ price trader wishes to buy assetA / sell assetB
    , scriptAddress :: LV2.Address
   -- ^ used to filter inputs under this script address
    }

--PlutusTx.makeIsDataIndexed ''OCOrderSide [('OCBuy, 0), ('OCSell, 1)]
PlutusTx.makeIsDataIndexed ''Dat [('Dat, 0)]

-- ---------------------------------------------------------------------- 
-- Redeemer
-- ---------------------------------------------------------------------- 

data Redeem 
    = Spend         -- spend utxo to execute a trade
        Integer     -- amount of asset to buy/sell in market order
        OrderSide -- is trader buying or selling 
    | Cancel        -- cancel this order and refund trader

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

-- TODO: Strategy for handling partially filled orders

{-# INLINEABLE mkValidator #-}
mkValidator :: Param -> Dat -> Redeem -> LV2.ScriptContext -> Bool 
mkValidator param dat red ctx = case red of 
    -- TODO: Reference input to check bid/ask prices

    Spend amountToTrade redSide ->
      case redSide of 

        -- TODO: Check utxo has NFT 
        
        -- UTXO has deposited AssetA and wants to trade for AssetB
        -- I.e. Deposited ADA to receive USD
        Buy -> 
          -- Tx is only consuming Sell UTXOs under this script address
          checkAllOwnScriptInputsOnSide Sell &&
          -- Check the tx matches the amount of assets to trade
          checkAmountToTrade amountToTrade   &&
          -- Check traderPkh receives datAmount of AssetB
          checkAmountSentToTraderAssetB 


        -- UTXO has deposited AssetB and wants to trade for AssetA
        -- I.e. Deposited USD to receive ADA
        Sell ->
          -- Tx is only consuming Buy UTXOs under this script address
          checkAllOwnScriptInputsOnSide Buy &&
          -- Check the tx matches the amount of assets to trade
          checkAmountToTrade amountToTrade  &&
          -- Check traderPkh receives datAmount of AssetA
          checkAmountSentToTraderAssetA


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
    signedByTrader = traceIfFalse "Wrong signer" $
      LV2C.txSignedBy txInfo $ L.unPaymentPubKeyHash (traderPkh dat)

    fullValueReturned :: Bool 
    fullValueReturned = LV2C.valuePaidTo txInfo pkh == ownInput ^. LTXV2.outValue
      where pkh = L.unPaymentPubKeyHash (traderPkh dat)

    -- ------------------------------------------------------------ 

    getInputs :: [LV2C.TxInInfo]
    getInputs = LV2C.txInfoInputs txInfo 

    checkAllOwnScriptInputsOnSide:: OrderSide -> Bool
    checkAllOwnScriptInputsOnSide s = not $ foldl (\b i -> if b then b else
      let txo = LV2.txInInfoResolved i in side (getDatum txo) /= s) False ownScriptInputs 
     
    ownScriptInputs :: [LV2.TxInInfo]
    ownScriptInputs = filter pred' getInputs
      where pred' i = LV2.txOutAddress (LV2.txInInfoResolved i) == scriptAddress dat

    -- ------------------------------------------------------------ 

    -- Check amount of asset A to be traded
    -- NOTE: Currently handles EXACT (==) matches (no partially filled orders)
    checkAmountToTrade :: Integer -> Bool
    checkAmountToTrade amt = amt == foldl (\acc i -> 
      acc + datAmount (getDatum (LV2.txInInfoResolved i))) 0 ownScriptInputs

    -- Check correct amount of asset A sent to trader
    checkAmountSentToTraderAssetA :: Bool 
    checkAmountSentToTraderAssetA =
      let expectedAssetA = V.assetClassValue (assetA param) (datAmount dat) 
      in LV2C.valuePaidTo txInfo 
          (L.unPaymentPubKeyHash $ traderPkh dat) == expectedAssetA

    -- Check correct amount of asset B sent to trader
    checkAmountSentToTraderAssetB :: Bool
    checkAmountSentToTraderAssetB = 
      let expectedAssetB = V.assetClassValue (assetB param) (datAmount dat * tradePrice dat)
      in LV2C.valuePaidTo txInfo (L.unPaymentPubKeyHash $ traderPkh dat) == expectedAssetB

-- ---------------------------------------------------------------------- 
-- Utilities
-- ---------------------------------------------------------------------- 
-- TODO: Put in utils module

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
  
-- ---------------------------------------------------------------------- 
-- Boilerplate
-- ---------------------------------------------------------------------- 

validatorInstance :: Param -> V2UtilsTypeScripts.TypedValidator Trade
validatorInstance = V2UtilsTypeScripts.mkTypedValidatorParam @Trade 
  $$(PlutusTx.compile [|| mkValidator ||])
  $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = V2UtilsTypeScripts.mkUntypedValidator @Dat @Redeem

scriptValidator :: Param -> LV2.Validator
scriptValidator = Scripts.validatorScript . validatorInstance

scriptValidatorHash :: Param -> LV2.ValidatorHash
scriptValidatorHash = V2UtilsTypeScripts.validatorHash . validatorInstance

scriptParamAddress :: Param -> L.Address
scriptParamAddress = L.scriptHashAddress . scriptValidatorHash

