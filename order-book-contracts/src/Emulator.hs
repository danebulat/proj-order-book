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
import OrderBook.Utils            (mkEmptyOrderBook)

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

tokenNameA :: LV2.TokenName
tokenNameA = "ASSET_A"

tokenNameB :: LV2.TokenName
tokenNameB = "ASSET_B"

tokenAssetClassA :: V.AssetClass
tokenAssetClassA = V.AssetClass (freeCurSymbol, tokenNameA)

tokenAssetClassB :: V.AssetClass
tokenAssetClassB = V.AssetClass (freeCurSymbol, tokenNameB)

-- ---------------------------------------------------------------------- 
-- Trace 1
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
    , alOrderBook  = mkEmptyOrderBook 
    }

  void $ waitNSlots 2

  -- Get updated order book
  newOb <- Emulator.observableState h1
  Extras.logInfo @String $ "UPDATED ORDER BOOK:\n" ++ show newOb

-- ---------------------------------------------------------------------- 
-- Trace 2
-- ---------------------------------------------------------------------- 

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
    , alOrderBook  = mkEmptyOrderBook
    }

  void $ waitNSlots 2
  
  -- Get updated order book
  newOb <- Emulator.observableState h1
  Extras.logInfo @String $ "UPDATED ORDER BOOK:\n" ++ show newOb

-- ---------------------------------------------------------------------- 
-- Trace 3
-- ---------------------------------------------------------------------- 

-- Combination of trace1 and trace2 to observe current bid/ask
trace3 :: Emulator.EmulatorTrace ()
trace3 = do 
  h1 <- Emulator.activateContractWallet (knownWallet 1) OffChain.contract
  h2 <- Emulator.activateContractWallet (knownWallet 2) OffChain.contract

  -- Set bid price @ 2
  Emulator.callEndpoint @"add-liquidity" h1 $ AddLiquidityArgs 
   { alAssetA     = tokenAssetClassA
   , alAssetB     = tokenAssetClassB 
   , alAmount     = 10 
   , alSide       = Buy 
   , alTradePrice = 2
   , alTraderAddr = mockWalletAddress (knownWallet 1)
   , alOrderBook  = mkEmptyOrderBook 
   }

  void $ waitNSlots 2
  ob1 <- Emulator.observableState h1
  void $ waitNSlots 2

  -- Set ask price @ 4
  Emulator.callEndpoint @"add-liquidity" h2 $ AddLiquidityArgs 
    { alAssetA     = tokenAssetClassA
    , alAssetB     = tokenAssetClassB 
    , alAmount     = 10 
    , alSide       = Sell 
    , alTradePrice = 4
    , alTraderAddr = mockWalletAddress (knownWallet 2)
    , alOrderBook  = head ob1 
    }

  void $ waitNSlots 2
  ob2 <- Emulator.observableState h2
  void $ waitNSlots 2
  Extras.logInfo @String $ "UPDATED ORDER BOOK:\n" ++ show ob2 

-- ---------------------------------------------------------------------- 
-- Trace 4
-- ---------------------------------------------------------------------- 

-- Wallet 1 Deposits 10xAssetA and Wallet 2 trades those 10xAssetA for 20xAssetB
trace4 :: Emulator.EmulatorTrace ()
trace4 = do 
  void $ waitNSlots 2

-- ---------------------------------------------------------------------- 
-- Test
-- ---------------------------------------------------------------------- 

test1 :: IO ()
test1 = Emulator.runEmulatorTraceIO' def emCfg trace1

test2 :: IO ()
test2 = Emulator.runEmulatorTraceIO' def emCfg trace2

test3 :: IO ()
test3 = Emulator.runEmulatorTraceIO' def emCfg trace3

