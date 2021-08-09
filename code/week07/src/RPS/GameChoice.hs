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

module RPS.GameChoice where

import           PlutusTx.Prelude             hiding (Semigroup(..), check, unless)
import qualified Prelude
import qualified PlutusTx
import           Data.Aeson                   (FromJSON, ToJSON)
import           GHC.Generics                 (Generic)
import           Playground.Contract          (ToSchema)
--import Data.Ord

data GameChoice = Rock | Paper | Scissors
    deriving (Prelude.Show, Generic, FromJSON, ToJSON, ToSchema, Prelude.Eq)

PlutusTx.unstableMakeIsData ''GameChoice

instance Eq GameChoice where
    {-# INLINABLE (==) #-}
    Rock == Rock = True
    Paper  == Paper  = True
    Scissors == Scissors = True
    _    == _    = False

instance Ord GameChoice where
    {-# INLINABLE (<=) #-}
    (<=) Rock Paper = True
    (<=) Paper Scissors = True
    (<=) Scissors Rock = True
    (<=) _ _ = False
    --compare Paper Scissors = LT
    --compare Scissors Rock = LT
    ----compare :: a -> a -> Ordering

beats :: GameChoice -> GameChoice -> Bool
beats Paper Rock = True
beats Rock Scissors = True
beats Scissors Paper = True
beats _ _ = False
