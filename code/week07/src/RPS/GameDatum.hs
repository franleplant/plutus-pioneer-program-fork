{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module RPS.GameDatum where

import           PlutusTx.Prelude             hiding (Semigroup(..), check, unless)
--import           Prelude                      (Semigroup (..), Show (..), String)
import qualified Prelude
import qualified PlutusTx
--import           Ledger                       hiding (singleton)
--import           Plutus.Contract.StateMachine
--import           Data.Aeson                   (FromJSON, ToJSON)
--import           GHC.Generics                 (Generic)
--import           Playground.Contract          (ToSchema)

import qualified RPS.GameChoice as GameChoice

data GameDatum = GameDatum ByteString (Maybe GameChoice.GameChoice) | Finished
    deriving Prelude.Show

instance Eq GameDatum where
    {-# INLINABLE (==) #-}
    GameDatum bs mc == GameDatum bs' mc' = (bs == bs') && (mc == mc')
    Finished        == Finished          = True
    _               == _                 = False

PlutusTx.unstableMakeIsData ''GameDatum

