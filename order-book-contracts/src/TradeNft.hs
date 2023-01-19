{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE ImportQualifiedPost   #-}

{-# OPTIONS_GHC -fno-warn-unused-imports   #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module TradeNft where

import Control.Lens                                    ((^.))
import PlutusTx 
import PlutusTx.Prelude 
import Ledger                                          qualified as L
import Plutus.V2.Ledger.Api                            qualified as LV2
import Plutus.V2.Ledger.Contexts                       qualified as LV2C 
import Plutus.V2.Ledger.Tx                             qualified as LTXV2
import Plutus.Script.Utils.V2.Scripts                  qualified as UtilsScriptsV2
import Plutus.Script.Utils.V2.Typed.Scripts.Validators qualified as UtilsScriptsV2 
import Plutus.Script.Utils.V2.Typed.Scripts            qualified as Scripts
import Prelude                                         qualified as P
import Plutus.V1.Ledger.Interval                       qualified as LV1Interval
import Plutus.V1.Ledger.Value                          qualified as V
import Ledger.Ada                                      qualified as Ada

import OnChain (Dat(..), OrderSide(..))

-- ---------------------------------------------------------------------- 
-- Parameter
-- ---------------------------------------------------------------------- 

data PolicyParam = PolicyParam 
    { ppAssetA        :: L.AssetClass 
    , ppAssetB        :: L.AssetClass 
    , ppScriptAddress :: LV2.Address
    }

PlutusTx.makeIsDataIndexed ''PolicyParam [('PolicyParam, 0)]
PlutusTx.makeLift ''PolicyParam

-- ---------------------------------------------------------------------- 
-- Policy
-- ---------------------------------------------------------------------- 

-- TODO: Reference input with datum containing the curBid + curAsk

{-# INLINEABLE mkPolicy #-}
mkPolicy :: PolicyParam -> LV2.TxOutRef -> LV2C.ScriptContext -> Bool
mkPolicy param utxo ctx = case mintedValue of 
    [(_cs, tn, n)] -> validateMint tn n
    _other         -> False
  where 
    txInfo :: LV2C.TxInfo
    txInfo = LV2C.scriptContextTxInfo ctx

    mintFlattened :: [(LV2.CurrencySymbol, LV2.TokenName, Integer)]
    mintFlattened = V.flattenValue (LV2C.txInfoMint txInfo)
    
    -- Check minted currency symbol is this validor script currency symbol
    mintedValue :: [(LV2.CurrencySymbol, LV2.TokenName, Integer)]
    mintedValue = filter (\(cs, _tn, _n) -> cs == LV2C.ownCurrencySymbol ctx) mintFlattened
    
    -- ------------------------------------------------------------ 
    -- Checks
    -- ------------------------------------------------------------ 

    -- [x] Check: Validate token name 
    -- [x] Check: Overflow 
    -- [x] Check: Minted amount is 1
    -- [x] Check: Utxo is being consumed
    -- [x] Check: Token being deposited in script utxo
    -- [x] Check: AssetA or AssetB is being deposited to script utxo only
    -- [ ] Check: Trade price is valid based on the current bid or ask price 

    validateMint :: LV2.TokenName -> Integer -> Bool 
    validateMint tn amount = 
         amount == 1 
      && checkForOverflow
      && validateTokenName tn
      && hasUTxO
      && checkScriptOutputValue
    
    -- Token name
    calculateTokenNameHash :: BuiltinByteString
    calculateTokenNameHash = sha2_256 (consByteString idx txid)
      where idx  = LV2C.txOutRefIdx utxo
            txid = LV2.getTxId $ LV2.txOutRefId utxo

    validateTokenName :: LV2.TokenName -> Bool
    validateTokenName tn = LV2.unTokenName tn == calculateTokenNameHash

    ownAssetClass :: L.AssetClass
    ownAssetClass = V.assetClass (LV2C.ownCurrencySymbol ctx) 
                                 (LV2.TokenName calculateTokenNameHash)
    
    -- Overflow
    checkForOverflow :: Bool
    checkForOverflow = LV2C.txOutRefIdx utxo < 256

    -- Redeemer utxo in inputs
    hasUTxO:: Bool
    hasUTxO = any (\i -> LV2C.txInInfoOutRef i == utxo) $ LV2C.txInfoInputs txInfo

    -- Get output being created at the script address
    getScriptUTxO :: LV2C.TxOut
    getScriptUTxO = 
      let targetAddr  = ppScriptAddress param
          txOuts      = LV2C.txInInfoResolved <$> LV2C.txInfoInputs txInfo
          scriptOuts  = filter (\txOut -> LV2C.txOutAddress txOut == targetAddr) txOuts
      in case scriptOuts of 
        [o]    -> o
        _other -> traceError "Expecting only one output to script address"

    --
    -- TODO: Check including minLovelace is the correct calculation
    --
    checkScriptOutputValue :: Bool 
    checkScriptOutputValue = case side scriptOutDat of 
        -- Check script output contains AssetB tokens and NFT token only (+ min lovelace)
        Buy -> 
          let numTokenB = V.assetClassValueOf scriptOutVal (ppAssetB param)
              targetVal = targetNftVal 
                     P.<> V.assetClassValue (ppAssetB param) numTokenB
                     P.<> minLovelace
          in targetVal == scriptOutVal 

        -- Check script output contains AssetA tokens and NFT token only (+ min lovelace)
        Sell -> 
          let numTokenA = V.assetClassValueOf scriptOutVal (ppAssetA param)
              targetVal = targetNftVal 
                     P.<> V.assetClassValue (ppAssetA param) numTokenA
                     P.<> minLovelace
          in targetVal == scriptOutVal
      where 
        scriptOut    = getScriptUTxO
        scriptOutDat = getDatum scriptOut
        scriptOutVal = LV2C.txOutValue scriptOut
        targetNftVal = V.assetClassValue ownAssetClass 1

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
