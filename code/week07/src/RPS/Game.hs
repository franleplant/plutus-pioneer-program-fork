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

module RPS.Game where

import           PlutusTx.Prelude             hiding (Semigroup(..), check, unless)
import qualified Prelude
import qualified PlutusTx
import           Ledger                       hiding (singleton)
import           Plutus.Contract.StateMachine
import           Data.Aeson                   (FromJSON, ToJSON)
import           GHC.Generics                 (Generic)

data Game = Game
    { firstPlayer          :: !PubKeyHash
    , secondPlayer         :: !PubKeyHash
    , stake                :: !Integer
    , playDeadline         :: !POSIXTime
    , revealDeadline       :: !POSIXTime
    , threadToken          :: !ThreadToken
    } deriving (Prelude.Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.makeLift ''Game
