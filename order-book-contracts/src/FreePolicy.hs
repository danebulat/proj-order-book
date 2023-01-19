{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE OverloadedStrings          #-}

module FreePolicy where

import PlutusTx                             qualified
import PlutusTx.Prelude                     hiding (pure, (<$>))
import Plutus.V2.Ledger.Api                 qualified as LV2
import Plutus.V2.Ledger.Contexts            qualified as LV2C

import Plutus.Script.Utils.V2.Typed.Scripts qualified as Scripts
import Plutus.Script.Utils.V2.Scripts       (scriptCurrencySymbol)

-- ---------------------------------------------------------------------- 
-- Free policy script
-- ---------------------------------------------------------------------- 

{-# INLINEABLE mkFreePolicy #-}
mkFreePolicy :: () -> LV2C.ScriptContext -> Bool
mkFreePolicy () _ = True

freePolicy :: Scripts.MintingPolicy
freePolicy = LV2.mkMintingPolicyScript 
    $$(PlutusTx.compile [|| Scripts.mkUntypedMintingPolicy mkFreePolicy ||])

freeCurSymbol :: LV2.CurrencySymbol 
freeCurSymbol = scriptCurrencySymbol freePolicy 
