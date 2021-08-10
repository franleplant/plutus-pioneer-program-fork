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
import qualified Prelude
import qualified PlutusTx

import qualified RPS.GameChoice as GameChoice

data GameDatum =
      State0 ByteString
    | State1 ByteString            GameChoice.GameChoice
    | State2 GameChoice.GameChoice GameChoice.GameChoice
    | StateFinal
    deriving Prelude.Show

instance Eq GameDatum where
    {-# INLINABLE (==) #-}
    State0 bs == State0 bs' =
        bs == bs'
    State1 bs player2Choice == State1 bs' player2Choice' =
        bs == bs' && player2Choice == player2Choice'
    State2 player1Choice player2Choice == State2 player1Choice' player2Choice' =
        player1Choice == player1Choice' && player2Choice == player2Choice'

    StateFinal == StateFinal = True
    _               == _                 = False

PlutusTx.unstableMakeIsData ''GameDatum

