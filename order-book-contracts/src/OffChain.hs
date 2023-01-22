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

import Control.Monad              (when)
import Control.Lens               ((^.))
import Data.Aeson                 (FromJSON, ToJSON)
import Data.Map                   (Map)
import Data.Map                   qualified as Map 
import Data.Maybe                 (fromJust)
import Data.Text                  qualified as T
import GHC.Generics               (Generic)
import Text.Printf                (printf)

import PlutusTx                   qualified
import PlutusTx.Prelude           hiding ((<>))
import Plutus.V2.Ledger.Api       qualified as LV2
import Plutus.V2.Ledger.Contexts  qualified as LV2C
import Plutus.V1.Ledger.Value     qualified as V
import Prelude                    qualified as P 
import Prelude                    ((<>))

import Ledger                     qualified as L
import Ledger.Ada                 qualified as Ada
import Ledger.Constraints         qualified as Constraints 
import Ledger.Tx                  qualified as LTX
import Plutus.Contract            (type (.\/))
import Plutus.Contract            qualified as PC 

import OnChain                    qualified 
import OnChain                    (Dat(..), Param(..), scriptParamAddress)
import TradeNft                   qualified
import TradeNft                   (PolicyParam(..))
import OrderBook.Model            (OrderSide(..), OrderBook(..), Order(..), OrderType(..))
import OrderBook.Matching         qualified as Matching
import OrderBook.Utils            (addLimitOrders, mkLimitOrder, getFlattenedOrders, removeLimitOrder)

-- ====================================================================== 
-- Contract endpoins
-- ====================================================================== 

-- ---------------------------------------------------------------------- 
-- "add-liquidity" contract endpoint
-- ---------------------------------------------------------------------- 

addLiquidity :: PC.Promise [OrderBook] TradeSchema T.Text ()
addLiquidity = PC.endpoint @"add-liquidity" $ \args -> do 
    
    -- Check if limit trade is valid
    validLimitPrice <- isValidLimitPrice args

    when (not validLimitPrice) $
      PC.logError @P.String $ printf "Invalid limit price"

    when validLimitPrice $ do
      -- Get wallet utxo to provide as redeemer in TradeNft policy
      utxos <- PC.utxosAt (alTraderAddr args)
      pkh   <- PC.ownFirstPaymentPubKeyHash

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
              nftAssetClass = V.assetClass (TradeNft.curSymbol policyParam) (LV2.TokenName tokenName)

          PC.logInfo @P.String $ printf "Value to deposit: %s" (P.show val)
          PC.logInfo @P.String $ printf "NFT to deposit: %s" (P.show nftVal)

          -- Construct tx
          let lookups = Constraints.typedValidatorLookups (OnChain.validatorInstance param)
                     <> Constraints.plutusV2MintingPolicy (TradeNft.policy policyParam)
                     <> Constraints.unspentOutputs utxos
             
              tx      = Constraints.mustPayToTheScriptWithInlineDatum dat (val P.<> nftVal)
                     <> Constraints.mustMintValueWithRedeemer policyRedeemer nftVal

          -- NOTE: Min lovelace added to tx when balancing
          PC.mkTxConstraints lookups tx >>= PC.adjustUnbalancedTx >>= PC.yieldUnbalancedTx
          
          -- Add NFT to order book along with order details
          let ob    = alOrderBook args
              order = mkLimitOrder (alTradePrice args) nftAssetClass pkh (alAmount args) (alSide args)
              newOb = addLimitOrders [order] ob 

          -- Send new order book outside contract monad
          PC.tell [newOb]
          PC.logInfo @P.String $ printf "DEPOSITED TO SCRIPT ADDRESS"

isValidLimitPrice :: AddLiquidityArgs -> PC.Contract [OrderBook] TradeSchema T.Text Bool
isValidLimitPrice args = 
  case s of 
    -- tPrice < ask 
    Buy ->  let mCurAsk = obCurAsk ob 
            in case mCurAsk of 
                 Nothing     -> return True 
                 Just curAsk -> return $ p < curAsk && incIsDivisible
    -- tPrice > bid
    Sell -> let mCurBid = obCurBid ob 
            in case mCurBid of 
                 Nothing     -> return True
                 Just curBid -> return $ p > curBid && incIsDivisible 
  where 
    p  = alTradePrice args
    s  = alSide       args 
    ob = alOrderBook  args
    incIsDivisible = p `P.mod` obIncrement ob == 0

getValueToPay :: AddLiquidityArgs -> PC.Contract [OrderBook] TradeSchema T.Text V.Value
getValueToPay args = case alSide args of
  Buy  -> return $ V.assetClassValue (alAssetB args) (alAmount args * alTradePrice args)
  Sell -> return $ V.assetClassValue (alAssetA args) (alAmount args) 

getDatumParamFromArgs :: AddLiquidityArgs -> PC.Contract [OrderBook] TradeSchema T.Text (Dat, Param)
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

removeLiquidity :: PC.Promise [OrderBook] TradeSchema T.Text ()
removeLiquidity = PC.endpoint @"remove-liquidity" $ \args -> do 
  pkh <- PC.ownFirstPaymentPubKeyHash
  utxosAtWallet <- PC.utxosAt (rlTraderAddr args)

  case Map.keys utxosAtWallet of
    [] -> PC.logError @P.String $ printf "No UTxOs found at wallet address"
    (walletOref: _) -> do

      -- Get NFT from order to cancel
      let walletOut     = utxosAtWallet Map.! walletOref -- consumed for policy redeemer
          orderBook     = rlOrderBook args
          walletOrders  = filter (\o -> oPkh o == pkh) (getFlattenedOrders orderBook)
          orderToCancel = walletOrders !! rlOrderIndex args
          orderNft      = getNftFromOrder orderToCancel

      -- Get the corresponding utxo on the blockchain
      let param = Param { assetA = rlAssetA args, assetB = rlAssetB args }
      utxosAtScript <- PC.utxosAt (OnChain.scriptParamAddress param)
      utxos         <- filterNftUTxOs utxosAtScript [orderNft]

      -- For policy script to burn NFT
      let policyRedeemer = L.Redeemer $ PlutusTx.toBuiltinData walletOref
          policyParams = PolicyParam 
            { ppAssetA = rlAssetA args 
            , ppAssetB = rlAssetB args 
            , ppScriptAddress = OnChain.scriptParamAddress param 
            }
          nftVal = V.assetClassValue orderNft 1

      -- Construct transaction to spend utxo and refund value back to this wallet
      let (oref, o) = head . Map.toList $ utxos

          lookups = Constraints.typedValidatorLookups (OnChain.validatorInstance param)
                 <> Constraints.plutusV2MintingPolicy (TradeNft.policy policyParams)
                 <> Constraints.unspentOutputs (Map.singleton oref o)
                 <> Constraints.unspentOutputs (Map.singleton walletOref walletOut)

          tx      = Constraints.mustSpendScriptOutput oref 
                    (L.Redeemer $ PlutusTx.toBuiltinData $ OnChain.Cancel orderNft) 
                 <> Constraints.mustPayToPubKey pkh (o ^. LTX.decoratedTxOutValue <> negate nftVal)
                 <> Constraints.mustBeSignedBy pkh
                 <> Constraints.mustMintValueWithRedeemer policyRedeemer (negate nftVal)

      -- Submit transaction
      PC.mkTxConstraints lookups tx >>= PC.adjustUnbalancedTx >>= PC.yieldUnbalancedTx

      -- Update order book (remove an order)
      let newOb = removeLimitOrder orderToCancel orderBook
      PC.tell [newOb]
      PC.logInfo @P.String $ printf "REMOVED LIQUIDITY"

-- ---------------------------------------------------------------------- 
-- "trade-assets" contract endpoint
-- ---------------------------------------------------------------------- 

tradeAssets :: PC.Promise [OrderBook] TradeSchema T.Text ()
tradeAssets = PC.endpoint @"trade-assets" $ \args -> do
  ownPkh <- PC.ownFirstPaymentPubKeyHash

  let targetAmt = taAmount args
      orderBook = taOrderBook args
      param     = Param { assetA = taAssetA args, assetB = taAssetB args }
      policyParams = PolicyParam 
           { ppAssetA = taAssetA args
           , ppAssetB = taAssetB args
           , ppScriptAddress = OnChain.scriptParamAddress param
           }

  case taSide args of
    Buy -> do
      PC.logInfo @P.String $ printf "ENTERING BUY MARKET ORDER" 

      -- Get order NFTs and script outputs to spend 
      (orderNfts, newOb) <- takeOrdersForBuy targetAmt orderBook 
      utxosAtScript      <- PC.utxosAt (OnChain.scriptParamAddress param)
      utxos              <- filterNftUTxOs utxosAtScript orderNfts
      
      -- Get data from filtered UTxOs (pkh, val, amount, oref, tradePrice)
      utxoData <- getUTxOData utxos orderNfts
      PC.logInfo @P.String $ printf "ORDER NFTS:\n" ++ P.show orderNfts 

      -- Construct transaction 
      let lookups = Constraints.unspentOutputs utxos
                 <> Constraints.typedValidatorLookups (OnChain.validatorInstance param)
                 <> Constraints.plutusV2MintingPolicy (TradeNft.policy policyParams)
           
          tx = mconcat [
                    -- Spend script outputs with correct redeemer  
                    Constraints.mustSpendScriptOutput oref 
                     (L.Redeemer $ PlutusTx.toBuiltinData $ OnChain.Spend amt Buy nftClass)  

                    -- Pay AssetB to each trader's pkh according to their trade 
                 <> Constraints.mustPayToPubKey pkh 
                      (V.assetClassValue (taAssetB args) (amt * tp))

                    -- Pay AssetA to own pkh (consume value of taken utxos)
                 <> Constraints.mustPayToPubKey ownPkh 
                      (v <> negate (V.assetClassValue nftClass 1))

                    -- Burn NFTs at script UTxOs
                 <> Constraints.mustMintValueWithRedeemer 
                     (L.Redeemer $ PlutusTx.toBuiltinData oref) 
                     (negate $ V.assetClassValue nftClass 1)
                    | (pkh, v, amt, oref, tp, nftClass) <- utxoData ]
                    -- NOTE: Min Lovelace also goes to own pkh

      -- Submit transaction and tell new order book
      PC.mkTxConstraints lookups tx >>= PC.adjustUnbalancedTx >>= PC.yieldUnbalancedTx
      PC.tell [newOb]

    Sell -> do 
      PC.logInfo @P.String $ printf "ENTERING SELL MARKET ORDER"

      -- Get order NFTs and script outputs to spend
      (orderNfts, newOb) <- takeOrdersForSell targetAmt orderBook
      utxosAtScript      <- PC.utxosAt (OnChain.scriptParamAddress param)
      utxos              <- filterNftUTxOs utxosAtScript orderNfts
      
      -- Get data from filtered UTxOs (pkh, val, amount, oref, tradePrice)
      utxoData <- getUTxOData utxos orderNfts

      -- Construct transaction
      let lookups = Constraints.unspentOutputs utxos 
                 <> Constraints.typedValidatorLookups (OnChain.validatorInstance param)
                 <> Constraints.plutusV2MintingPolicy (TradeNft.policy policyParams)

          tx = mconcat [ 
                    -- Spend script outputs with correct redeemer 
                    Constraints.mustSpendScriptOutput oref 
                      (L.Redeemer $ PlutusTx.toBuiltinData $ OnChain.Spend amt Sell nftClass)  

                    -- Pay AssetA to each trader's pkh according to their trade 
                 <> Constraints.mustPayToPubKey pkh 
                      (V.assetClassValue (taAssetA args) amt)

                    -- Pay AssetB to own pkh (consume value of taken utxos)
                 <> Constraints.mustPayToPubKey ownPkh 
                      (v <> negate (V.assetClassValue nftClass 1))

                    -- Burn NFTs at script UTxOs
                 <> Constraints.mustMintValueWithRedeemer 
                      (L.Redeemer $ PlutusTx.toBuiltinData oref) 
                      (negate $ V.assetClassValue nftClass 1)
                     
                    | (pkh, v, amt, oref, _, nftClass) <- utxoData ]
                    -- NOTE: Min Lovelace also goes to own pkh

      -- Submit transaction and tell new order book
      PC.mkTxConstraints lookups tx >>= PC.adjustUnbalancedTx >>= PC.yieldUnbalancedTx
      PC.tell [newOb]

-- Take orders from order book for BUY market order
-- Return associated NFTs and updated order book
takeOrdersForBuy 
  :: Integer -> OrderBook 
  -> PC.Contract [OrderBook] TradeSchema T.Text ([V.AssetClass], OrderBook)
takeOrdersForBuy targetAmt ob = do
  let curAsk         = fromJust $ obCurAsk ob
      (_, os, newOb) = Matching.takeOrdersForBuy curAsk targetAmt ob (0,[])
  return (getNftFromOrder <$> os, newOb)

-- Return outputs that contain one of the passed NFTs
filterNftUTxOs 
  :: Map LTX.TxOutRef LTX.DecoratedTxOut -> [V.AssetClass] 
  -> PC.Contract [OrderBook] TradeSchema T.Text (Map LTX.TxOutRef LTX.DecoratedTxOut)
filterNftUTxOs utxos nfts = return $ 
  Map.filter (\decTxOut -> 
        let v = decTxOut ^. LTX.decoratedTxOutValue 
        in foldl (\b nft -> if b then b else V.assetClassValueOf v nft == 1) False nfts) 
      utxos 

-- Get data from filtered UTxOs to construct the transaction
getUTxOData 
  :: Map LTX.TxOutRef LTX.DecoratedTxOut 
  -> [L.AssetClass]                                  -- order NFTs 
  -> PC.Contract [OrderBook] TradeSchema T.Text 
      [(L.PaymentPubKeyHash, V.Value, Integer, LTX.TxOutRef, Integer, L.AssetClass)]
getUTxOData utxos nfts = return $ foldl (\acc (oref, decTxOut) ->
  let v  = decTxOut ^. LTX.decoratedTxOutValue
      (_, dq) = LTX._decoratedTxOutScriptDatum decTxOut 
  in case dq of 
    LTX.DatumInline d -> 
      case PlutusTx.fromBuiltinData (LV2.getDatum d) of 
        Just d' -> acc ++ [ (OnChain.traderPkh d'    -- trader pkh
                          , v                        -- utxo value
                          , OnChain.datAmount  d'    -- amount of asset A trading
                          , oref                     -- outref
                          , OnChain.tradePrice d'    -- price to trade
                          , getNft v) ]              -- utxo nft asset class
        Nothing -> acc
    _other -> acc) [] (Map.toList utxos)
    where 
      getNft v = P.foldl1 (\acc nft -> if V.assetClassValueOf v nft == 1 then nft else acc) nfts

-- Take orders from order book for SELL market order
-- Return associated NFTs and updated order book
takeOrdersForSell 
  :: Integer -> OrderBook 
  -> PC.Contract [OrderBook] TradeSchema T.Text ([V.AssetClass], OrderBook)
takeOrdersForSell targetAmt ob = do 
  let curBid         = fromJust $ obCurBid ob
      (_, os, newOb) = Matching.takeOrdersForSell curBid targetAmt ob (0,[])
  return (getNftFromOrder <$> os, newOb)

-- ---------------------------------------------------------------------- 
-- Top-level contract entry point
-- ---------------------------------------------------------------------- 

contract :: PC.Contract [OrderBook] TradeSchema T.Text ()
contract = do 
  PC.logInfo @P.String "Waiting for add-liquidity endpoint"
  PC.selectList [addLiquidity, removeLiquidity, tradeAssets] >> contract

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
  , alOrderBook     :: OrderBook
  } 
  deriving stock (P.Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data RemoveLiquidityArgs = RemoveLiquidityArgs
  { rlAssetA     :: L.AssetClass 
  , rlAssetB     :: L.AssetClass 
  , rlOrderBook  :: OrderBook
  , rlOrderIndex :: Integer
  , rlTraderAddr :: LV2.Address
 -- ^ which order to cancel (user may have multiple orders in order book)
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

