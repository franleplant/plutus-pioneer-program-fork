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

module RPS where

import           Control.Monad                hiding (fmap)
import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Monoid                  (Last (..))
import           Data.Text                    (Text, pack)
import           GHC.Generics                 (Generic)
import           Ledger                       hiding (singleton)
import           Ledger.Ada                   as Ada
import           Ledger.Constraints           as Constraints
import           Ledger.Typed.Tx
import qualified Ledger.Typed.Scripts         as Scripts
import           Plutus.Contract              as Contract
import           Plutus.Contract.StateMachine
import qualified PlutusTx
import           PlutusTx.Prelude             hiding (Semigroup(..), check, unless)
import           Playground.Contract          (ToSchema)
import           Prelude                      (Semigroup (..), Show (..), String)
import qualified Prelude

import qualified RPS.Game as Game
import qualified RPS.GameChoice as GameChoice
import qualified RPS.GameDatum as GameDatum
import qualified RPS.GameRedeemer as GameRedeemer
import qualified RPS.Validator as Validator


gameClient :: Game.Game -> StateMachineClient GameDatum.GameDatum GameRedeemer.GameRedeemer
gameClient game = mkStateMachineClient $ StateMachineInstance (Validator.gameStateMachine' game) (Validator.typedGameValidator game)
