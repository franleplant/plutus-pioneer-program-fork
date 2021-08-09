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

module RPS.Validator where

import           Control.Monad                hiding (fmap)
import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Monoid                  (Last (..))
import           Data.Text                    (Text, pack)
import           GHC.Generics                 (Generic)
import           Ledger                       hiding (singleton)
import           Ledger.Ada                   as Ada
import           Ledger.Constraints           as Constraints
import qualified Ledger.Typed.Scripts         as Scripts
import           Ledger.Typed.Tx
import           Playground.Contract          (ToSchema)
import           Plutus.Contract              as Contract
import           Plutus.Contract.StateMachine
import qualified PlutusTx
import           PlutusTx.Prelude             hiding (Semigroup (..), check,
                                               unless)
import           Prelude                      (Semigroup (..), Show (..),
                                               String)
import qualified Prelude

import qualified RPS.Game                     as Game
import qualified RPS.GameChoice               as GameChoice
import qualified RPS.GameDatum                as GameDatum
import qualified RPS.GameRedeemer             as GameRedeemer

ROCK, PAPER, SCISSORS :: ByteString
ROCK = "rock"
PAPER  = "paper"
SCISSORS  = "scissors"

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

{-# INLINABLE getDatum #-}
getDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe GameDatum.GameDatum
getDatum output f = do
    dh      <- txOutDatum output
    Datum d <- f dh
    PlutusTx.fromBuiltinData d

{-# INLINABLE transition #-}
transition :: Game.Game
           -> State GameDatum.GameDatum
           -> GameRedeemer.GameRedeemer
           -> Maybe (TxConstraints Void Void, State GameDatum.GameDatum)

transition game state redeemer = case (redeemer, stateValue state, stateData state) of
    (GameRedeemer.FirstPlayerClaim, staked, GameDatum.GameDatum _ Nothing)
        | lovelaces staked == Game.stake game         -> Just ( Constraints.mustBeSignedBy (Game.firstPlayer game)                     <>
                                                       Constraints.mustValidateIn (from $ 1 + Game.playDeadline game)
                                                     , State GameDatum.Finished mempty
                                                     )
    (GameRedeemer.FirstPlayerReveal _, staked, GameDatum.GameDatum _ (Just _))
        | lovelaces staked == (2 * Game.stake game)   -> Just ( Constraints.mustBeSignedBy (Game.firstPlayer game)                     <>
                                                       Constraints.mustValidateIn (to $ Game.revealDeadline game)
                                                     , State GameDatum.Finished mempty
                                                     )
    (GameRedeemer.SecondPlayerPlay choice, staked, GameDatum.GameDatum bytes Nothing)
        | lovelaces staked == Game.stake game         -> Just ( Constraints.mustBeSignedBy (Game.secondPlayer game)                    <>
                                                       Constraints.mustValidateIn (to $ Game.playDeadline game)
                                                     , State (GameDatum.GameDatum bytes $ Just choice) (lovelaceValueOf $ 2 * Game.stake game)
                                                     )
    (GameRedeemer.SecondPlayerClaim, staked, GameDatum.GameDatum _ (Just _))
        | lovelaces staked == (2 * Game.stake game)   -> Just ( Constraints.mustBeSignedBy (Game.secondPlayer game)                    <>
                                                       Constraints.mustValidateIn (from $ 1 + Game.revealDeadline game)
                                                     , State GameDatum.Finished mempty
                                                     )
                                                     -- TODO
    --(GameRedeemer.Draw, staked, GameDatum.GameDatum _ (Just _))
        -- | lovelaces staked == (2 * Game.stake game)   -> Just ( Constraints.mustBeSignedBy (Game.secondPlayer game)                    <>
                                                       --Constraints.mustValidateIn (from $ 1 + Game.revealDeadline game)
                                                     --, State GameDatum.Finished mempty
                                                     --)
    _                                        -> Nothing

{-# INLINABLE final #-}
final :: GameDatum.GameDatum -> Bool
final GameDatum.Finished = True
final _                  = False

{-# INLINABLE check #-}
check :: ByteString
      -> ByteString
      -> ByteString
      -> GameDatum.GameDatum
      -> GameRedeemer.GameRedeemer
      -> ScriptContext
      -> Bool
check rock paper scissors (GameDatum.GameDatum bytes (Just secondPlayerChoice)) (GameRedeemer.FirstPlayerReveal nonce) _ =
    sha2_256 (nonce `concatenate` if c == GameChoice.Zero then bsZero' else bsOne') == bs
check _       _      _                       _              _ = True

{-# INLINABLE gameStateMachine #-}
gameStateMachine :: Game.Game -> ByteString -> ByteString -> ByteString -> StateMachine GameDatum.GameDatum GameRedeemer.GameRedeemer
gameStateMachine game rock paper scissors = StateMachine
    { smTransition  = transition game
    , smFinal       = final
    , smCheck       = check rock paper scissors
    , smThreadToken = Just $ Game.threadToken game
    }

{-# INLINABLE mkGameValidator #-}
mkGameValidator :: Game.Game -> ByteString -> ByteString -> ByteString -> GameDatum.GameDatum -> GameRedeemer.GameRedeemer -> ScriptContext -> Bool
mkGameValidator game rock paper scissors = mkValidator $ gameStateMachine game rock paper scissors

type Gaming = StateMachine GameDatum.GameDatum GameRedeemer.GameRedeemer


gameStateMachine' :: Game.Game -> StateMachine GameDatum.GameDatum GameRedeemer.GameRedeemer
gameStateMachine' game = gameStateMachine game bsZero bsOne

typedGameValidator :: Game.Game -> Scripts.TypedValidator Gaming
typedGameValidator game = Scripts.mkTypedValidator @Gaming
    ($$(PlutusTx.compile [|| mkGameValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode game
        `PlutusTx.applyCode` PlutusTx.liftCode ROCK
        `PlutusTx.applyCode` PlutusTx.liftCode PAPER
        `PlutusTx.applyCode` PlutusTx.liftCode SCISSORS)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @GameDatum.GameDatum @GameRedeemer.GameRedeemer

gameValidator :: Game.Game -> Validator
gameValidator = Scripts.validatorScript . typedGameValidator

gameAddress :: Game.Game -> Ledger.Address
gameAddress = scriptAddress . gameValidator

