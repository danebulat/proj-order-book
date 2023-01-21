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
import OffChain                   (AddLiquidityArgs(..), TradeAssetsArgs(..), RemoveLiquidityArgs(..))
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
      , (w4, v)
      ]
    }

v :: V.Value
v = Ada.lovelaceValueOf 100_000_000        <>
    V.assetClassValue tokenAssetClassA 100 <>
    V.assetClassValue tokenAssetClassB 100

w1, w2, w3, w4 :: Wallet
w1 = knownWallet 1
w2 = knownWallet 2
w3 = knownWallet 3
w4 = knownWallet 4

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
  Extras.logInfo @String $ "UPDATED ORDER BOOK:\n" ++ show (head newOb)

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
  Extras.logInfo @String $ "UPDATED ORDER BOOK:\n" ++ show (head newOb)

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
  Extras.logInfo @String $ "UPDATED ORDER BOOK:\n" ++ show (last ob2)

-- ---------------------------------------------------------------------- 
-- Trace 4
-- ---------------------------------------------------------------------- 

-- Setup same as trace3 followed by a market BUY order
trace4 :: Emulator.EmulatorTrace ()
trace4 = do 
  h1 <- Emulator.activateContractWallet (knownWallet 1) OffChain.contract
  h2 <- Emulator.activateContractWallet (knownWallet 2) OffChain.contract
  h3 <- Emulator.activateContractWallet (knownWallet 3) OffChain.contract

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

  -- Wallet3 Market Order (Buy 10xAssetA costing 40xAssetB)
  Emulator.callEndpoint @"trade-assets" h3 $ TradeAssetsArgs 
    { taAssetA    = tokenAssetClassA 
    , taAssetB    = tokenAssetClassB 
    , taAmount    = 10   -- Must match a seller amount EXACTLY
    , taSide      = Buy 
    , taOrderBook = last ob2 
    }

  void $ waitNSlots 2
  ob3 <- Emulator.observableState h3 
  Extras.logInfo @String $ "UPDATED ORDER BOOK:\n" ++ show ob3

-- ---------------------------------------------------------------------- 
-- Trace 5
-- ---------------------------------------------------------------------- 

trace5 :: Emulator.EmulatorTrace ()
trace5 = do
  h1 <- Emulator.activateContractWallet (knownWallet 1) OffChain.contract
  h2 <- Emulator.activateContractWallet (knownWallet 2) OffChain.contract
  h3 <- Emulator.activateContractWallet (knownWallet 3) OffChain.contract
  h4 <- Emulator.activateContractWallet (knownWallet 4) OffChain.contract

  -- Buy 10xAssetA @ 2
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
  Extras.logInfo @String $ "OBSERVABLE STATE 01:\n" ++ show ob1
  void $ waitNSlots 2

  -- Sell 10xAssetA @ 4
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
  Extras.logInfo @String $ "OBSERVABLE STATE 02:\n" ++ show ob2
  void $ waitNSlots 2

  -- Sell 10xAssetA @ 4
  Emulator.callEndpoint @"add-liquidity" h3 $ AddLiquidityArgs 
    { alAssetA     = tokenAssetClassA
    , alAssetB     = tokenAssetClassB 
    , alAmount     = 10 
    , alSide       = Sell 
    , alTradePrice = 4
    , alTraderAddr = mockWalletAddress (knownWallet 3)
    , alOrderBook  = last ob2 
    }
  void $ waitNSlots 2
  ob3 <- Emulator.observableState h3
  Extras.logInfo @String $ "OBSERVABLE STATE 03:\n" ++ show (last ob3)
  void $ waitNSlots 2

  -- Buy 20XAssetA (market order)
  Emulator.callEndpoint @"trade-assets" h4 $ TradeAssetsArgs 
    { taAssetA    = tokenAssetClassA 
    , taAssetB    = tokenAssetClassB 
    , taAmount    = 20   -- Must match seller amount(s) EXACTLY
    , taSide      = Buy 
    , taOrderBook = last ob3 
    }

  void $ waitNSlots 2
  ob4 <- Emulator.observableState h4 
  void $ waitNSlots 2
  Extras.logInfo @String $ "UPDATED ORDER BOOK:\n" ++ show (last ob4)

-- ---------------------------------------------------------------------- 
-- Trace 6
-- ---------------------------------------------------------------------- 

-- Deposit 20xAssetB and cancel BUY order
trace6 :: Emulator.EmulatorTrace ()
trace6 = do 
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
  ob1 <- Emulator.observableState h1
  void $ waitNSlots 2

  Emulator.callEndpoint @"remove-liquidity" h1 $ RemoveLiquidityArgs
    { rlAssetA     = tokenAssetClassA
    , rlAssetB     = tokenAssetClassB
    , rlOrderBook  = head ob1 
    , rlOrderIndex = 0
    , rlTraderAddr = mockWalletAddress (knownWallet 1)
    }
  
  void $ waitNSlots 2
  ob2 <- Emulator.observableState h1
  void $ waitNSlots 2
  Extras.logInfo @String $ "UPDATED ORDER BOOK:\n" ++ show (last ob2)

-- ---------------------------------------------------------------------- 
-- Trace 7
-- ---------------------------------------------------------------------- 

-- Deposit 10xAssetA and cancel SELL order
trace7 :: Emulator.EmulatorTrace ()
trace7 = do 
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
  ob1 <- Emulator.observableState h1
  void $ waitNSlots 2

  Emulator.callEndpoint @"remove-liquidity" h1 $ RemoveLiquidityArgs
    { rlAssetA     = tokenAssetClassA
    , rlAssetB     = tokenAssetClassB
    , rlOrderBook  = head ob1 
    , rlOrderIndex = 0
    , rlTraderAddr = mockWalletAddress (knownWallet 1)
    }
  
  void $ waitNSlots 2
  ob2 <- Emulator.observableState h1
  void $ waitNSlots 2
  Extras.logInfo @String $ "UPDATED ORDER BOOK:\n" ++ show (last ob2)


-- ---------------------------------------------------------------------- 
-- Test
-- ---------------------------------------------------------------------- 

test1 :: IO ()
test1 = Emulator.runEmulatorTraceIO' def emCfg trace1

test2 :: IO ()
test2 = Emulator.runEmulatorTraceIO' def emCfg trace2

test3 :: IO ()
test3 = Emulator.runEmulatorTraceIO' def emCfg trace3

test4 :: IO ()
test4 = Emulator.runEmulatorTraceIO' def emCfg trace4

test5 :: IO ()
test5 = Emulator.runEmulatorTraceIO' def emCfg trace5

test6 :: IO ()
test6 = Emulator.runEmulatorTraceIO' def emCfg trace6

test7 :: IO ()
test7 = Emulator.runEmulatorTraceIO' def emCfg trace7
