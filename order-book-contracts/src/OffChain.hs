{-# LANGUAGE DataKinds                     #-}
{-# LANGUAGE DeriveAnyClass                #-}
{-# LANGUAGE DeriveGeneric                 #-}
{-# LANGUAGE DerivingStrategies            #-}
{-# LANGUAGE ImportQualifiedPost           #-}
{-# LANGUAGE NoImplicitParams              #-}
{-# LANGUAGE OverloadedStrings             #-}
{-# LANGUAGE TypeApplications              #-}
{-# LANGUAGE TypeOperators                 #-}

{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module OffChain where

import Control.Lens                   ((^.))
import Data.Aeson                     (FromJSON, ToJSON)
--import Data.Map                       (Map)
import Data.Map                       qualified as Map 
import Data.Maybe                     (fromJust)
import Data.Text                      qualified as T
import GHC.Generics                   (Generic)
import Text.Printf                    (printf)

import PlutusTx                       qualified
import PlutusTx.Prelude
import Plutus.V2.Ledger.Api           qualified as LV2
import Plutus.V2.Ledger.Contexts      qualified as LV2C
import Plutus.V1.Ledger.Value         qualified as V
import Prelude                        qualified as P 

import Ledger                         qualified as L
import Ledger.Ada                     qualified as Ada
import Ledger.Constraints             qualified as Constraints 
import Ledger.Tx                      qualified as LTX
import Plutus.Contract                (type (.\/))
import Plutus.Contract                qualified as PC 

import OnChain                        qualified 
import OnChain                        (Dat(..), Param(..), scriptParamAddress)
import TradeNft                       qualified
import TradeNft                       (PolicyParam(..))
import OrderBook.Model                (OrderSide(..), OrderBook(..), Order(..), OrderType(..))
import OrderBook.Matching             qualified as Matching

-- ====================================================================== 
-- Contract endpoins
-- ====================================================================== 

-- ---------------------------------------------------------------------- 
-- "add-liquidity" contract endpoint
-- ---------------------------------------------------------------------- 

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
        
        -- TODO: Add NFT to order book along with order details
        -- TODO: Send order book outside of functions

        PC.logInfo @P.String $ printf "Deposited to script address"

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
tradeAssets = PC.endpoint @"trade-assets" $ \args -> do
  ownPkh <- PC.ownFirstPaymentPubKeyHash

  case taSide args of
    Buy -> do
      let targetAmt      = taAmount args
          ob             = taOrderBook args
          curAsk         = fromJust $ obCurAsk ob
          validatorParam = OnChain.Param (taAssetA args) (taAssetB args)
          param          = Param { assetA = taAssetA args, assetB = taAssetB args }

      -- Take orders from order book to consume
      let (_amtToConsume, os, _newOb) = Matching.takeOrdersForBuy curAsk targetAmt ob (0,[])

      -- Get NFTs from extracted orders
          orderNfts = getNftFromOrder <$> os

      -- Retrieve corresponding on-chain UTxOs
      -- utxosAt :: Address -> Map TxOutRef DecoratedTxOut
      utxosAtScript <- PC.utxosAt (OnChain.scriptParamAddress validatorParam)
      
      -- Filter utxos holding the NFTs
      let _utxos = Map.filter (\decTxOut -> 
                    let v = decTxOut ^. LTX.decoratedTxOutValue 
                    in foldl (\b nft -> if b then b else V.assetClassValueOf v nft == 1) False orderNfts) 
                  utxosAtScript 
        
          -- Get data from filtered UTxOs (pkh, val, amount, oref, tradePrice)
          _utxoData = foldl (\acc (oref, decTxOut) ->
                      let v  = decTxOut ^. LTX.decoratedTxOutValue
                          (dh, dq) = LTX._decoratedTxOutScriptDatum decTxOut 
                          in case dq of 
                            LTX.DatumInline d -> 
                              let md = PlutusTx.fromBuiltinData (LV2.getDatum d) 
                              in case md of 
                                Just d' -> acc ++ [ (OnChain.traderPkh d'    -- trader pkh
                                                  , v                        -- utxo value
                                                  , OnChain.datAmount  d'    -- amount of asset A trading
                                                  , oref                     -- outref
                                                  , OnChain.tradePrice d')]  -- price to trade
                                Nothing -> acc
                            _other -> acc) [] (Map.toList _utxos)
          
          _lookups = 
                   Constraints.unspentOutputs _utxos
              P.<> Constraints.typedValidatorLookups (OnChain.validatorInstance param)
          
          _tx =    -- Spend script outputs with correct redeemer 
                   [ Constraints.mustSpendScriptOutput oref 
                       (L.Redeemer $ PlutusTx.toBuiltinData $ OnChain.Spend amt Buy)  
                     | (pkh, v, amt, oref, tp) <- _utxoData 
                   ]
                   -- Pay AssetB to each trader's pkh according to their trade 
              P.<> [ Constraints.mustPayToPubKey pkh' 
                       (V.assetClassValue (taAssetB args) (amt * tp))
                     | (pkh', v, amt, oref, tp) <- _utxoData 
                   ]
                   -- Pay AssetA to own pkh (consume value of taken utxos)
                   -- NOTE: Min Lovelace also goes to own pkh
              P.<> [ Constraints.mustPayToPubKey ownPkh v 
                     | (pkh', v, amt, oref, tp) <- _utxoData 
                   ]

      PC.logInfo @P.String $ printf "BUY market order (%d of AssetA)" (taAmount args)

    Sell -> do 
      PC.logInfo @P.String $ printf "SELL market order (%d of AssetA)" (taAmount args)

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

getNftFromOrder :: Order -> V.AssetClass
getNftFromOrder = otlNft . oType 

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
  deriving anyclass (ToJSON, FromJSON)

data RemoveLiquidityArgs = RemoveLiquidityArgs
  { rlAssetA        :: L.AssetClass 
  , rlAssetB        :: L.AssetClass 
  }
  deriving stock (P.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data TradeAssetsArgs = TradeAssetsArgs 
  { taAssetA    :: L.AssetClass 
  , taAssetB    :: L.AssetClass
  , taAmount    :: Integer       -- Amount of asset to buy/sell
  , taSide      :: OrderSide     -- Side of currency pair to trade (buy or sell)
  , taOrderBook :: OrderBook     -- Order book to take orders from
  }
  deriving stock (P.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

