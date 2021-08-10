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

import           Ledger                       hiding (singleton)
import           Ledger.Ada                   as Ada
import           Ledger.Constraints           as Constraints
import qualified Ledger.Typed.Scripts         as Scripts
import           Plutus.Contract.StateMachine
import qualified PlutusTx
import           PlutusTx.Prelude             hiding (Semigroup (..), check,
                                               unless)
import           Prelude                      (Semigroup (..))

import qualified RPS.Game                     as Game
import qualified RPS.GameChoice               as GameChoice
import qualified RPS.GameDatum                as GameDatum
import qualified RPS.GameRedeemer             as GameRedeemer


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

transition game state redeemer = case (redeemer, stateData state, stateValue state ) of
    (GameRedeemer.Player1NoPlayClaim, GameDatum.GameDatum _ Nothing, staked)
      | lovelaces staked == Game.stake game       -> let constraints = Constraints.mustBeSignedBy (Game.player1 game) <>
                                                                       Constraints.mustValidateIn (from $ 1 + Game.playDeadline game)
                                                     in Just (constraints, State GameDatum.Finished mempty)

    (GameRedeemer.Player2Play choice, GameDatum.GameDatum bytes Nothing, staked)
      | lovelaces staked == Game.stake game       -> let constraints = Constraints.mustBeSignedBy (Game.player2 game) <>
                                                                       Constraints.mustValidateIn (to $ Game.playDeadline game)
                                                         datum       = GameDatum.GameDatum bytes $ Just choice
                                                         stake       = (lovelaceValueOf $ 2 * Game.stake game)
                                                     in Just (constraints , State datum stake)

    (GameRedeemer.Player1RevealWin _ _, GameDatum.GameDatum _ (Just _), staked)
      | lovelaces staked == (2 * Game.stake game) -> let constraints = Constraints.mustBeSignedBy (Game.player1 game) <>
                                                                       Constraints.mustValidateIn (to $ Game.revealDeadline game) <>
                                                                       Constraints.mustPayToPubKey (Game.player1 game) (lovelaceValueOf $ 2 * Game.stake game)
                                                     in Just (constraints, State GameDatum.Finished mempty)

    (GameRedeemer.Player2NoRevealClaim, GameDatum.GameDatum _ (Just _), staked)
      | lovelaces staked == (2 * Game.stake game) -> let constraints = Constraints.mustBeSignedBy (Game.player2 game)  <>
                                                                       Constraints.mustValidateIn (from $ 1 + Game.revealDeadline game)

                                                     in  Just ( constraints , State GameDatum.Finished mempty)

    (GameRedeemer.Player1RevealDraw _ _, GameDatum.GameDatum _ (Just _), staked)
      | lovelaces staked == (2 * Game.stake game) -> let constraints =  Constraints.mustBeSignedBy (Game.player1 game)  <>
                                                                        Constraints.mustValidateIn (to $ Game.revealDeadline game) <>
                                                                        Constraints.mustPayToPubKey (Game.player1 game) (lovelaceValueOf $ Game.stake game) <>
                                                                        Constraints.mustPayToPubKey (Game.player2 game) (lovelaceValueOf $ Game.stake game)

                                                     in  Just ( constraints , State GameDatum.Finished mempty)

    _                                             -> Nothing

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
check rock paper scissors (GameDatum.GameDatum player1ChoiceHash (Just player2Choice)) (GameRedeemer.Player1RevealWin nonce player1Choice) _ =
    traceIfFalse "player1 should reveal the original choice"
    --traceIfFalse ("player1 should reveal the original choice: " ++ (map charToString player1ChoiceHash) ++ "actual: " ++ presentedPlayer1ChoiceHash)
        (player1ChoiceHash == presentedPlayer1ChoiceHash) &&
    traceIfFalse "player1 beats player2" (GameChoice.beats player1Choice player2Choice)
        where
            player1ChoiceString :: ByteString
            player1ChoiceString = case player1Choice of
                GameChoice.Rock     -> rock
                GameChoice.Paper    -> paper
                GameChoice.Scissors -> scissors

            presentedPlayer1ChoiceHash :: ByteString
            presentedPlayer1ChoiceHash =  choiceHash
                where
                    choiceHash = sha2_256 (nonce `concatenate` player1ChoiceString)

check rock paper scissors (GameDatum.GameDatum player1ChoiceHash (Just player2Choice)) (GameRedeemer.Player1RevealDraw nonce player1Choice) _ =
    player1ChoiceHash == presentedPlayer1ChoiceHash &&
    player1Choice == player2Choice
        where
            player1ChoiceString :: ByteString
            player1ChoiceString = case player1Choice of
                GameChoice.Rock     -> rock
                GameChoice.Paper    -> paper
                GameChoice.Scissors -> scissors

            presentedPlayer1ChoiceHash :: ByteString
            presentedPlayer1ChoiceHash =  sha2_256 (nonce `concatenate` player1ChoiceString)

check _ _ _ _ _ _ = True

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
gameStateMachine' game = gameStateMachine game GameChoice.bsRock GameChoice.bsPaper GameChoice.bsScissors

typedGameValidator :: Game.Game -> Scripts.TypedValidator Gaming
typedGameValidator game = Scripts.mkTypedValidator @Gaming
    ($$(PlutusTx.compile [|| mkGameValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode game
        `PlutusTx.applyCode` PlutusTx.liftCode GameChoice.bsRock
        `PlutusTx.applyCode` PlutusTx.liftCode GameChoice.bsPaper
        `PlutusTx.applyCode` PlutusTx.liftCode GameChoice.bsScissors)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @GameDatum.GameDatum @GameRedeemer.GameRedeemer

gameValidator :: Game.Game -> Validator
gameValidator = Scripts.validatorScript . typedGameValidator

gameAddress :: Game.Game -> Ledger.Address
gameAddress = scriptAddress . gameValidator

