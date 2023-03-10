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

import Control.Lens              ((^.))
import GHC.Generics              (Generic)
import PlutusTx                  qualified
import PlutusTx.AssocMap         (keys)
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
    deriving (P.Show)

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

PlutusTx.makeIsDataIndexed ''Dat [('Dat, 0)]

-- ---------------------------------------------------------------------- 
-- Redeemer
-- ---------------------------------------------------------------------- 

data Redeem 
    = Spend         
   -- ^ spend utxo to execute a trade
        Integer      
     -- ^ amount of asset to buy/sell in market order
        OrderSide
     -- ^ whether trader buying or selling 
        L.AssetClass
     -- ^ this output's nft (checks burning)
    | Cancel         
   -- ^ cancel this order and refund trader
        L.AssetClass

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
mkValidator param dat red ctx = case red of 

    Spend amountToTrade redSide nftClass ->
      case redSide of 

        -- UTXO has deposited AssetA and wants to trade for AssetB
        -- I.e. Deposited ADA to receive USD
        Buy -> 
          -- Tx is only consuming Sell UTXOs under this script address
          checkAllOwnScriptInputsOnSide Sell &&
          -- Check the tx matches the amount of assets to trade
          checkAmountToTrade amountToTrade   &&
          -- Check traderPkh receives datAmount of AssetB
          checkAmountSentToTraderAssetB      &&
          -- Check if this output's nft is burnt
          isBurningNft nftClass 

        -- UTXO has deposited AssetB and wants to trade for AssetA
        -- I.e. Deposited USD to receive ADA
        Sell ->
          -- Tx is only consuming Buy UTXOs under this script address
          checkAllOwnScriptInputsOnSide Buy &&
          -- Check the tx matches the amount of assets to trade
          checkAmountToTrade amountToTrade  &&
          -- Check traderPkh receives datAmount of AssetA
          checkAmountSentToTraderAssetA     &&
          -- Check if this output's nft is burnt
          isBurningNft nftClass 

    -- Trader wishes to cancel this order and get back their deposit
    Cancel nftAssetClass -> 
      signedByTrader && fullValueReturned nftAssetClass && isBurningNft nftAssetClass
  
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

    fullValueReturned :: L.AssetClass -> Bool 
    fullValueReturned nftAssetClass = traceIfFalse "Full value not returned" $ 
        LV2C.valuePaidTo txInfo pkh `V.geq` (ownInput ^. LTXV2.outValue <> negate (V.assetClassValue nftAssetClass 1))
      where pkh = L.unPaymentPubKeyHash (traderPkh dat)

    getInputs :: [LV2C.TxInInfo]
    getInputs = LV2C.txInfoInputs txInfo 

    checkAllOwnScriptInputsOnSide:: OrderSide -> Bool
    checkAllOwnScriptInputsOnSide s = traceIfFalse "Inputs have wrong side" $ 
        not $ foldl (\b i -> if b then b else
                let txo = LV2.txInInfoResolved i in side (getDatum txo) /= s) 
              False ownScriptInputs 
     
    ownScriptInputs :: [LV2.TxInInfo]
    ownScriptInputs = filter pred' getInputs
      where pred' i = LV2.txOutAddress (LV2.txInInfoResolved i) == scriptAddress dat

    -- Check amount of assetA to be traded matches at least one input 
    -- NOTE: Currently handles EXACT (==) matches (no partially filled orders)
    checkAmountToTrade :: Integer -> Bool
    checkAmountToTrade amt = traceIfFalse "Wrong amounts" $ 
      amt `elem` map (datAmount . getDatum . LV2.txInInfoResolved) ownScriptInputs

    -- Check correct amount of asset A sent to trader
    checkAmountSentToTraderAssetA :: Bool 
    checkAmountSentToTraderAssetA = traceIfFalse "Wrong amount sent to utxo trader A" $
      let expectedAssetA = V.assetClassValue (assetA param) (datAmount dat) 
      in LV2C.valuePaidTo txInfo 
          (L.unPaymentPubKeyHash $ traderPkh dat) `V.geq` expectedAssetA
      -- NOTE: `geq` being used as trader gets min lovelace as well as assets

    -- Check correct amount of asset B sent to trader
    checkAmountSentToTraderAssetB :: Bool
    checkAmountSentToTraderAssetB = traceIfFalse "Wrong amount sent to utxo trader B" $
      let expectedAssetB = V.assetClassValue (assetB param) (datAmount dat * tradePrice dat)
      in LV2C.valuePaidTo txInfo 
          (L.unPaymentPubKeyHash $ traderPkh dat) `V.geq` expectedAssetB
      -- NOTE: `geq` being used as trader gets min lovelace as well as assets

    -- Check burning of NFT
    isBurningNft :: L.AssetClass -> Bool 
    isBurningNft nftAssetClass = traceIfFalse "NFT not burnt" $
      let mintVal = LV2C.txInfoMint txInfo
      in if mintVal == mempty
          then traceError "No value burnt"
          -- Multiple NFTs can be burnt in a single transaction, so check that 
          -- this output's NFT is one of the values being burnt.
          else V.assetClassValueOf mintVal nftAssetClass == (-1) 

-- ---------------------------------------------------------------------- 
-- Utilities
-- ---------------------------------------------------------------------- 

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

