{-# LANGUAGE AllowAmbiguousTypes           #-}
{-# LANGUAGE DataKinds                     #-}
{-# LANGUAGE DeriveAnyClass                #-}
{-# LANGUAGE DeriveGeneric                 #-}
{-# LANGUAGE DerivingStrategies            #-}
{-# LANGUAGE FlexibleContexts              #-}
{-# LANGUAGE ImportQualifiedPost           #-}
{-# LANGUAGE MultiParamTypeClasses         #-}
{-# LANGUAGE NamedFieldPuns                #-}
{-# LANGUAGE NoImplicitPrelude             #-}
{-# LANGUAGE PartialTypeSignatures         #-}
{-# LANGUAGE ScopedTypeVariables           #-}
{-# LANGUAGE TypeApplications              #-}
{-# LANGUAGE TypeFamilies                  #-}
{-# LANGUAGE TypeOperators                 #-}
{-# LANGUAGE OverloadedStrings             #-}

{-# OPTIONS_GHC -fno-warn-unused-imports   #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module OffChain where

import Control.Monad                  (void)
import Data.Aeson                     (FromJSON, ToJSON)
import Data.Map                       (Map)
import Data.Map                       qualified as Map 
import Data.Maybe                     (catMaybes, fromJust)
import Data.Text                      qualified as T
import GHC.Generics                   (Generic)
import Text.Printf                    (printf)

import PlutusTx                       qualified
import PlutusTx.Prelude               hiding (pure, (<$>))
import Plutus.V2.Ledger.Api           qualified as LV2
import Plutus.V2.Ledger.Contexts      qualified as LV2C
import Plutus.V1.Ledger.Value         qualified as V
import Prelude                        qualified as P 

import Ledger                         qualified as L
import Ledger.Ada                     qualified as Ada
import Ledger.Address                 qualified as V1LAddress
import Ledger.Constraints             qualified as Constraints 
import Ledger.Typed.Scripts           qualified as Scripts
import Ledger.Tx                      qualified as LTx
import Playground.Contract            (ToSchema)
import Plutus.Contract                (type (.\/))
import Plutus.Contract                qualified as PC 

import OnChain                       qualified 
import OnChain                       (OrderSide(..), Dat(..), Param(..))

-- ---------------------------------------------------------------------- 
-- Schema
-- ---------------------------------------------------------------------- 

type TradeSchema =
      PC.Endpoint "add-liquidity" AddLiquidityArgs

-- ---------------------------------------------------------------------- 
-- Data types for contract arguments
-- ---------------------------------------------------------------------- 

-- | Arguments for the "add-liquidity endpoint"
data AddLiquidityArgs = AddLiquidityArgs
  { alAssetA        :: L.AssetClass 
  , alAssetB        :: L.AssetClass 
  , alTraderPkh     :: L.PaymentPubKeyHash 
  , alAmount        :: Integer
  , alSide          :: OrderSide
  , alTradePrice    :: Integer 
  , alScriptAddress :: LV2.Address
  } 
  deriving stock (P.Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- ====================================================================== 
-- Contract endpoins
-- ====================================================================== 

-- ---------------------------------------------------------------------- 
-- "add-liquidity" contract endpoint
-- ---------------------------------------------------------------------- 

getValueToPay :: AddLiquidityArgs -> PC.Contract () TradeSchema T.Text V.Value
getValueToPay args = case alSide args of
  Buy  -> return $ V.assetClassValue (alAssetB args) (alAmount args * alTradePrice args)
  Sell -> return $ V.assetClassValue (alAssetA args) (alAmount args) 

getDatumParamFromArgs :: AddLiquidityArgs -> PC.Contract () TradeSchema T.Text (Dat, Param)
getDatumParamFromArgs args = do
    pkh <- PC.ownFirstPaymentPubKeyHash
    return (dat pkh, param) 
  where 
    param   = Param { assetA = alAssetA args, assetB = alAssetB args }
    addr    = OnChain.scriptParamAddress param
    dat pkh = Dat { traderPkh     = pkh
                  , datAmount     = alAmount args
                  , side          = alSide args
                  , tradePrice    = alTradePrice args 
                  , scriptAddress = addr
                  }

addLiquidity :: PC.Promise () TradeSchema T.Text ()
addLiquidity = PC.endpoint @"add-liquidity" $ \args -> do 
    val          <- getValueToPay args
    (dat, param) <- getDatumParamFromArgs args
    let
      lookups = Constraints.typedValidatorLookups (OnChain.validatorInstance param)
      tx      = Constraints.mustPayToTheScriptWithInlineDatum dat val 
    PC.mkTxConstraints lookups tx >>= PC.adjustUnbalancedTx >>= PC.yieldUnbalancedTx
    PC.logInfo @P.String $ printf "Depositing to script address"

-- ---------------------------------------------------------------------- 
-- Top-level contract entry point
-- ---------------------------------------------------------------------- 

contract :: PC.Contract () TradeSchema T.Text ()
contract = do 
  PC.logInfo @P.String "Waiting for add-liquidity endpoint"
  PC.selectList [addLiquidity] >> contract

-- ---------------------------------------------------------------------- 
-- Helper functions
-- ---------------------------------------------------------------------- 

minLovelace :: V.Value
minLovelace = Ada.lovelaceValueOf (Ada.getLovelace  L.minAdaTxOut)

