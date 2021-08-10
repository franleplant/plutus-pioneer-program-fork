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
import qualified Prelude
import qualified PlutusTx

import qualified RPS.GameChoice as GameChoice

data GameRedeemer =
        Player2Play GameChoice.GameChoice
      | Player2NoRevealClaim
      | Player1RevealWin ByteString GameChoice.GameChoice
      | Player1RevealDraw ByteString GameChoice.GameChoice
      | Player2ClaimDraw
      | Player1NoPlayClaim
    deriving Prelude.Show

PlutusTx.unstableMakeIsData ''GameRedeemer
