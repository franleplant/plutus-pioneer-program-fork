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

module RPS.GameRedeemer where

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

data GameRedeemer =
        Player2Play GameChoice.GameChoice
      | Player2NoRevealClaim
      | Player1RevealWin ByteString
      | Player1RevealDraw ByteString
      | Player1NoPlayClaim
    deriving Prelude.Show

PlutusTx.unstableMakeIsData ''GameRedeemer
