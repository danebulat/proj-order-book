{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ImportQualifiedPost  #-}
{-# LANGUAGE NumericUnderscores   #-}
{-# LANGUAGE OverloadedStrings    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports   #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Emulator where

import Plutus.Trace.Emulator      qualified as Emulator
import Data.Default               (Default (..))
import Data.Map                   qualified as Map
import Control.Monad.Freer.Extras as Extras
import Data.Functor               (void)
import Plutus.Trace
import Plutus.V2.Ledger.Api       qualified as LV2
import Plutus.V1.Ledger.Value     qualified as V
import Ledger                     qualified as L
import Ledger.Ada                 qualified as Ada
import Ledger.TimeSlot            qualified as TimeSlot
import Wallet.Emulator.Wallet     (Wallet, knownWallet, mockWalletPaymentPubKeyHash, mockWalletAddress)
import Wallet.Emulator.Wallet     qualified as Wallet

import OffChain                   qualified 
import OffChain                   (AddLiquidityArgs(..))
import OnChain                    qualified
import FreePolicy                 (freeCurSymbol)
import OrderBook.Model            (OrderSide(..))

-- ---------------------------------------------------------------------- 
-- Configuration
-- ---------------------------------------------------------------------- 

-- Wallets start with 100 ADA, 100 ASSET_A, 100 ASSET_B
emCfg :: Emulator.EmulatorConfig
emCfg = def {
    Emulator._initialChainState = Left $ Map.fromList
      [ (w1, v)
      , (w2, v)
      , (w3, v)
      ]
    }

v :: V.Value
v = Ada.lovelaceValueOf 100_000_000        <>
    V.assetClassValue tokenAssetClassA 100 <>
    V.assetClassValue tokenAssetClassB 100

w1, w2, w3 :: Wallet
w1 = knownWallet 1
w2 = knownWallet 2
w3 = knownWallet 3

-- ---------------------------------------------------------------------- 
-- Helper functions
-- ---------------------------------------------------------------------- 

-- TODO: Implement decimal places to do calculations like 
-- 1xAssetA = 0.5xAssetB

tokenNameA :: LV2.TokenName
tokenNameA = "ASSET_A"

tokenNameB :: LV2.TokenName
tokenNameB = "ASSET_B"

tokenAssetClassA :: V.AssetClass
tokenAssetClassA = V.AssetClass (freeCurSymbol, tokenNameA)

tokenAssetClassB :: V.AssetClass
tokenAssetClassB = V.AssetClass (freeCurSymbol, tokenNameB)

-- ---------------------------------------------------------------------- 
-- Trace 
-- ---------------------------------------------------------------------- 

-- Deposit 20xAssetB to BUY 10xAssetA
trace1 :: Emulator.EmulatorTrace ()
trace1 = do 
  h1 <- Emulator.activateContractWallet (knownWallet 1) OffChain.contract

  Emulator.callEndpoint @"add-liquidity" h1 $ AddLiquidityArgs 
    { alAssetA     = tokenAssetClassA
    , alAssetB     = tokenAssetClassB 
    , alAmount     = 10 
    , alSide       = Buy 
    , alTradePrice = 2
    , alTraderAddr = mockWalletAddress (knownWallet 1)
    }

  void $ waitNSlots 2

-- Deposit 10xAssetA to SELL for 20xAssetB
trace2 :: Emulator.EmulatorTrace ()
trace2 = do 
  h1 <- Emulator.activateContractWallet (knownWallet 1) OffChain.contract

  Emulator.callEndpoint @"add-liquidity" h1 $ AddLiquidityArgs 
    { alAssetA     = tokenAssetClassA
    , alAssetB     = tokenAssetClassB 
    , alAmount     = 10 
    , alSide       = Sell 
    , alTradePrice = 2
    , alTraderAddr = mockWalletAddress (knownWallet 1)
    }

  void $ waitNSlots 2

-- ---------------------------------------------------------------------- 
-- Test
-- ---------------------------------------------------------------------- 

test :: IO ()
test = Emulator.runEmulatorTraceIO' def emCfg trace2
