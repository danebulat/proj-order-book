{-# LANGUAGE AllowAmbiguousTypes           #-}
{-# LANGUAGE DataKinds                     #-}
{-# LANGUAGE DeriveAnyClass                #-}
{-# LANGUAGE DeriveGeneric                 #-}
{-# LANGUAGE DerivingStrategies            #-}
{-# LANGUAGE FlexibleContexts              #-}
{-# LANGUAGE ImportQualifiedPost           #-}
{-# LANGUAGE MultiParamTypeClasses         #-}
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
import OnChain                       (OrderSide(..), Dat(..), Param(..), scriptParamAddress)
import TradeNft                      qualified
import TradeNft                      (PolicyParam(..))

-- ---------------------------------------------------------------------- 
-- Schema
-- ---------------------------------------------------------------------- 

type TradeSchema =
        PC.Endpoint "add-liquidity"    AddLiquidityArgs
    .\/ PC.Endpoint "remove-liquidity" RemoveLiquidityArgs
    .\/ PC.Endpoint "trade-assets"     TradeAssetsArgs

-- ---------------------------------------------------------------------- 
-- Data types for contract arguments
-- ---------------------------------------------------------------------- 

-- | Arguments for the "add-liquidity endpoint"
data AddLiquidityArgs = AddLiquidityArgs
  { alAssetA        :: L.AssetClass 
  , alAssetB        :: L.AssetClass 
  , alAmount        :: Integer
  , alSide          :: OrderSide
  , alTradePrice    :: Integer 
  , alTraderAddr    :: LV2.Address
  } 
  deriving stock (P.Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data RemoveLiquidityArgs = RemoveLiquidityArgs
  { rlAssetA        :: L.AssetClass 
  , rlAssetB        :: L.AssetClass 
  }
  deriving stock (P.Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TradeAssetsArgs = TradeAssetsArgs 
  { taAssetA :: L.AssetClass 
  , taAssetB :: L.AssetClass
  , taAmount :: Integer       -- Amount of asset to buy/sell
  , taSide   :: OrderSide     -- Side of currency pair to trade (buy or sell)
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

--let utxo = LTx.toTxInfoTxOut oref

addLiquidity :: PC.Promise () TradeSchema T.Text ()
addLiquidity = PC.endpoint @"add-liquidity" $ \args -> do 
    
    -- Get wallet utxo to provide as redeemer in TradeNft policy
    utxos <- PC.utxosAt (alTraderAddr args)

    -- Return if no utxos available to spend
    case Map.keys utxos of 
      [] -> PC.logError @P.String $ printf "No UTxOs found at wallet address"
      (oref : _) -> do 
        
        -- For output at script address
        (dat, param) <- getDatumParamFromArgs args
        val <- getValueToPay args

        -- For minting Trade NFT
        let policyRedeemer = L.Redeemer $ PlutusTx.toBuiltinData oref
            tokenName      = calculateTokenNameHash oref
            policyParam    = PolicyParam 
              { ppAssetA        = alAssetA args
              , ppAssetB        = alAssetB args 
              , ppScriptAddress = OnChain.scriptParamAddress param
              }
            nftVal = LV2.singleton (TradeNft.curSymbol policyParam) (LV2.TokenName tokenName) 1

        PC.logInfo @P.String $ printf "Value to deposit: %s" (P.show val)
        PC.logInfo @P.String $ printf "NFT to deposit: %s" (P.show nftVal)

        -- Construct tx
        let
          lookups = Constraints.typedValidatorLookups (OnChain.validatorInstance param)
               P.<> Constraints.plutusV2MintingPolicy (TradeNft.policy policyParam)
               P.<> Constraints.unspentOutputs utxos
           
          tx      = Constraints.mustPayToTheScriptWithInlineDatum dat (val P.<> nftVal)
               P.<> Constraints.mustMintValueWithRedeemer policyRedeemer nftVal

        -- NOTE: Min lovelace added to tx when balancing
        PC.mkTxConstraints lookups tx >>= PC.adjustUnbalancedTx >>= PC.yieldUnbalancedTx
        PC.logInfo @P.String $ printf "Deposited to script address"

-- ---------------------------------------------------------------------- 
-- "remove-liquidity" contract endpoint
-- ---------------------------------------------------------------------- 

removeLiquidity :: PC.Promise () TradeSchema T.Text ()
removeLiquidity = PC.endpoint @"remove-liquidity" $ \_args -> do 
  PC.logInfo @P.String $ printf "Removed liquidity"

-- ---------------------------------------------------------------------- 
-- "trade-assets" contract endpoint
-- ---------------------------------------------------------------------- 

tradeAssets :: PC.Promise () TradeSchema T.Text ()
tradeAssets = PC.endpoint @"trade-assets" $ \_args -> do 
  PC.logInfo @P.String $ printf "Traded assets"

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

calculateTokenNameHash :: LV2.TxOutRef -> BuiltinByteString
calculateTokenNameHash oref = sha2_256 (consByteString idx txid)
  where idx  = LV2C.txOutRefIdx oref 
        txid = LV2.getTxId $ LV2.txOutRefId oref 

