{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ImportQualifiedPost  #-}
{-# LANGUAGE NumericUnderscores   #-}
{-# LANGUAGE OverloadedStrings    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports   #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Spec.Trace where

import Plutus.Trace.Emulator      qualified as Emulator
import Plutus.Contract.Test       qualified as PCT
import Plutus.Contract.Test       ((.&&.), (.||.))
import Test.Tasty
import PlutusTx.Numeric           qualified as N (negate)

import Control.Lens               ((&), (.~))
import Control.Monad.Freer.Extras as Extras
import Data.Default               (Default (..))
import Data.Map                   qualified as Map
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
import OnChain                    (Param(..))
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

minLovelaceVal :: V.Value
minLovelaceVal = Ada.toValue L.minAdaTxOut

tradeScriptAddress :: L.Address
tradeScriptAddress = OnChain.scriptParamAddress $ Param tokenAssetClassA tokenAssetClassB

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

predicate1 :: PCT.TracePredicate
predicate1 = PCT.walletFundsChange w1 
               (  N.negate (V.assetClassValue tokenAssetClassB 20)
               )

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

predicate2 :: PCT.TracePredicate
predicate2 = PCT.walletFundsChange w1 
               (  N.negate (V.assetClassValue tokenAssetClassA 10)
               )

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

predicate3 :: PCT.TracePredicate
predicate3 = 
       PCT.walletFundsChange w1 
       (  N.negate (V.assetClassValue tokenAssetClassB 20)
       )
  .&&. PCT.walletFundsChange w2 
       (  N.negate (V.assetClassValue tokenAssetClassA 10) 
       )

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

predicate4 :: PCT.TracePredicate
predicate4 = 
       PCT.walletFundsChange w1 
       (  N.negate (V.assetClassValue tokenAssetClassB 20)
       )
  .&&. PCT.walletFundsChange w2 
       (  N.negate (V.assetClassValue tokenAssetClassA 10) 
       <>           V.assetClassValue tokenAssetClassB 40
       )
  .&&. PCT.walletFundsChange w3
       (            V.assetClassValue tokenAssetClassA 10
       <> N.negate (V.assetClassValue tokenAssetClassB 40)  
       )

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

predicate5 :: PCT.TracePredicate
predicate5 = 
       PCT.walletFundsChange w1 
       (  N.negate (V.assetClassValue tokenAssetClassB 20)
       )
  .&&. PCT.walletFundsChange w2 
       (  N.negate (V.assetClassValue tokenAssetClassA 10) 
       <>           V.assetClassValue tokenAssetClassB 40
       )
  .&&. PCT.walletFundsChange w3
       (  N.negate (V.assetClassValue tokenAssetClassA 10)
       <>           V.assetClassValue tokenAssetClassB 40
       )
  .&&. PCT.walletFundsChange w4
       (            V.assetClassValue tokenAssetClassA 20
       <> N.negate (V.assetClassValue tokenAssetClassB 80)
       )

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

predicate6 :: PCT.TracePredicate
predicate6 = 
       PCT.walletFundsChange w1 mempty

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

predicate7 :: PCT.TracePredicate
predicate7 = 
       PCT.walletFundsChange w1 mempty

-- ---------------------------------------------------------------------- 
-- Trace 8
-- ---------------------------------------------------------------------- 

trace8 :: Emulator.EmulatorTrace ()
trace8 = do
  h1 <- Emulator.activateContractWallet (knownWallet 1) OffChain.contract
  h2 <- Emulator.activateContractWallet (knownWallet 2) OffChain.contract
  h3 <- Emulator.activateContractWallet (knownWallet 3) OffChain.contract
  h4 <- Emulator.activateContractWallet (knownWallet 4) OffChain.contract

  -- Limit order: Sell 10xAssetA @ 3
  Emulator.callEndpoint @"add-liquidity" h1 $ AddLiquidityArgs 
   { alAssetA     = tokenAssetClassA
   , alAssetB     = tokenAssetClassB 
   , alAmount     = 5 
   , alSide       = Sell
   , alTradePrice = 3
   , alTraderAddr = mockWalletAddress (knownWallet 1)
   , alOrderBook  = mkEmptyOrderBook 
   }

  void $ waitNSlots 2
  ob1 <- Emulator.observableState h1
  Extras.logInfo @String $ "OBSERVABLE STATE 01:\n" ++ show ob1
  void $ waitNSlots 2

  -- Limit Order: Sell 5xAssetA @ 4
  Emulator.callEndpoint @"add-liquidity" h2 $ AddLiquidityArgs 
    { alAssetA     = tokenAssetClassA
    , alAssetB     = tokenAssetClassB 
    , alAmount     = 5 
    , alSide       = Sell 
    , alTradePrice = 4
    , alTraderAddr = mockWalletAddress (knownWallet 2)
    , alOrderBook  = head ob1 
    }
  void $ waitNSlots 2
  ob2 <- Emulator.observableState h2 
  Extras.logInfo @String $ "OBSERVABLE STATE 02:\n" ++ show ob2
  void $ waitNSlots 2

  -- Limit Order: Sell 15xAssetA @ 4
  Emulator.callEndpoint @"add-liquidity" h3 $ AddLiquidityArgs 
    { alAssetA     = tokenAssetClassA
    , alAssetB     = tokenAssetClassB 
    , alAmount     = 3 
    , alSide       = Sell 
    , alTradePrice = 4
    , alTraderAddr = mockWalletAddress (knownWallet 3)
    , alOrderBook  = last ob2 
    }
  void $ waitNSlots 2
  ob3 <- Emulator.observableState h3
  Extras.logInfo @String $ "OBSERVABLE STATE 03:\n" ++ show (last ob3)
  void $ waitNSlots 2

  -- Market Order: Buy 15XAssetA
  Emulator.callEndpoint @"trade-assets" h4 $ TradeAssetsArgs 
    { taAssetA    = tokenAssetClassA 
    , taAssetB    = tokenAssetClassB 
    , taAmount    = 10   -- Must match seller amount(s) EXACTLY
    , taSide      = Buy 
    , taOrderBook = last ob3 
    }

  void $ waitNSlots 2
  ob4 <- Emulator.observableState h4 
  void $ waitNSlots 2
  Extras.logInfo @String $ "UPDATED ORDER BOOK:\n" ++ show (last ob4)

predicate8 :: PCT.TracePredicate
predicate8 = 
       PCT.walletFundsChange w1 
       (  N.negate (V.assetClassValue tokenAssetClassA 5)
       <>           V.assetClassValue tokenAssetClassB 15
       )
  .&&. PCT.walletFundsChange w2 
       (  N.negate (V.assetClassValue tokenAssetClassA 5) 
       <>           V.assetClassValue tokenAssetClassB 20
       )
  .&&. PCT.walletFundsChange w3
       (  N.negate (V.assetClassValue tokenAssetClassA 3)
       )
  .&&. PCT.walletFundsChange w4
       (            V.assetClassValue tokenAssetClassA 10
       <> N.negate (V.assetClassValue tokenAssetClassB 35)
       )

-- ---------------------------------------------------------------------- 
-- Trace 9
-- ---------------------------------------------------------------------- 

trace9 :: Emulator.EmulatorTrace ()
trace9 = do
  h1 <- Emulator.activateContractWallet (knownWallet 1) OffChain.contract
  h2 <- Emulator.activateContractWallet (knownWallet 2) OffChain.contract
  h3 <- Emulator.activateContractWallet (knownWallet 3) OffChain.contract
  h4 <- Emulator.activateContractWallet (knownWallet 4) OffChain.contract

  -- Limit order: Sell 10xAssetA @ 3
  Emulator.callEndpoint @"add-liquidity" h1 $ AddLiquidityArgs 
   { alAssetA     = tokenAssetClassA
   , alAssetB     = tokenAssetClassB 
   , alAmount     = 5 
   , alSide       = Buy
   , alTradePrice = 4
   , alTraderAddr = mockWalletAddress (knownWallet 1)
   , alOrderBook  = mkEmptyOrderBook 
   }

  void $ waitNSlots 2
  ob1 <- Emulator.observableState h1
  Extras.logInfo @String $ "OBSERVABLE STATE 01:\n" ++ show ob1
  void $ waitNSlots 2

  -- Limit Order: Sell 5xAssetA @ 4
  Emulator.callEndpoint @"add-liquidity" h2 $ AddLiquidityArgs 
    { alAssetA     = tokenAssetClassA
    , alAssetB     = tokenAssetClassB 
    , alAmount     = 5 
    , alSide       = Buy
    , alTradePrice = 3
    , alTraderAddr = mockWalletAddress (knownWallet 2)
    , alOrderBook  = head ob1 
    }
  void $ waitNSlots 2
  ob2 <- Emulator.observableState h2 
  Extras.logInfo @String $ "OBSERVABLE STATE 02:\n" ++ show ob2
  void $ waitNSlots 2

  -- Limit Order: Sell 15xAssetA @ 4
  Emulator.callEndpoint @"add-liquidity" h3 $ AddLiquidityArgs 
    { alAssetA     = tokenAssetClassA
    , alAssetB     = tokenAssetClassB 
    , alAmount     = 3 
    , alSide       = Buy
    , alTradePrice = 3
    , alTraderAddr = mockWalletAddress (knownWallet 3)
    , alOrderBook  = last ob2 
    }
  void $ waitNSlots 2
  ob3 <- Emulator.observableState h3
  Extras.logInfo @String $ "OBSERVABLE STATE 03:\n" ++ show (last ob3)
  void $ waitNSlots 2

  -- Market Order: Buy 15XAssetA
  Emulator.callEndpoint @"trade-assets" h4 $ TradeAssetsArgs 
    { taAssetA    = tokenAssetClassA 
    , taAssetB    = tokenAssetClassB 
    , taAmount    = 10   -- Must match seller amount(s) EXACTLY
    , taSide      = Sell
    , taOrderBook = last ob3 
    }

  void $ waitNSlots 2
  ob4 <- Emulator.observableState h4 
  void $ waitNSlots 2
  Extras.logInfo @String $ "UPDATED ORDER BOOK:\n" ++ show (last ob4)

predicate9 :: PCT.TracePredicate
predicate9 = 
       PCT.walletFundsChange w1 
       (            V.assetClassValue tokenAssetClassA 5
       <> N.negate (V.assetClassValue tokenAssetClassB 20)
       )
  .&&. PCT.walletFundsChange w2 
       (            V.assetClassValue tokenAssetClassA 5
       <> N.negate (V.assetClassValue tokenAssetClassB 15)
       )
  .&&. PCT.walletFundsChange w3
       (  N.negate (V.assetClassValue tokenAssetClassB 9)
       )
  .&&. PCT.walletFundsChange w4
       (  N.negate (V.assetClassValue tokenAssetClassA 10)
       <>           V.assetClassValue tokenAssetClassB 35
       )

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

test8 :: IO ()
test8 = Emulator.runEmulatorTraceIO' def emCfg trace8

test9 :: IO ()
test9 = Emulator.runEmulatorTraceIO' def emCfg trace9

-- ---------------------------------------------------------------------- 
-- Tests
-- ---------------------------------------------------------------------- 

testOptions :: PCT.CheckOptions
testOptions = PCT.defaultCheckOptions & PCT.emulatorConfig .~ emCfg

tests :: TestTree 
tests = testGroup "Trading Wallet Tests"
  [ PCT.checkPredicateOptions testOptions "Add Liquidity:    Buy Order"              predicate1 trace1
  , PCT.checkPredicateOptions testOptions "Add Liquidity:    Sell Order"             predicate2 trace2
  , PCT.checkPredicateOptions testOptions "Add Liquidity:    Buy & Sell Order"       predicate3 trace3
  , PCT.checkPredicateOptions testOptions "Trade Assets:     Consumes 1 Script UTxO" predicate4 trace4
  , PCT.checkPredicateOptions testOptions "Trade Assets:     Consumes 2 Script UTxO" predicate5 trace5
  , PCT.checkPredicateOptions testOptions "Remove Liquidity: Cancel Buy Order"       predicate6 trace6
  , PCT.checkPredicateOptions testOptions "Remove Liquidity: Cancel Sell Order"      predicate7 trace7

  , PCT.checkPredicateOptions testOptions "Trade Assets: Buy market order (consume 2 price levels)" 
                              predicate8 trace8
  , PCT.checkPredicateOptions testOptions "Trade Assets: Sell market order (consume 2 price levels)" 
                              predicate9 trace9
  ]

runTests :: IO ()
runTests = defaultMain tests
