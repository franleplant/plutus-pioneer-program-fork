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
import           RPS.GameDatum               hiding (GameDatum)
import qualified RPS.GameRedeemer as GameRedeemer
import qualified RPS.Validator as Validator


gameClient :: Game.Game -> StateMachineClient GameDatum.GameDatum GameRedeemer.GameRedeemer
gameClient game = mkStateMachineClient $ StateMachineInstance (Validator.gameStateMachine' game) (Validator.typedGameValidator game)

data Player1Params = Player1Params
    { fpSecond         :: !PubKeyHash
    , fpStake          :: !Integer
    , fpPlayDeadline   :: !POSIXTime
    , fpRevealDeadline :: !POSIXTime
    , fpNonce          :: !ByteString
    , fpChoice         :: !GameChoice.GameChoice
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

mapError' :: Contract w s SMContractError a -> Contract w s Text a
mapError' = mapError $ pack . show

waitUntilTimeHasPassed :: AsContractError e => POSIXTime -> Contract w s e ()
waitUntilTimeHasPassed t = void $ awaitTime t >> waitNSlots 1

firstGame :: forall s. Player1Params -> Contract (Last ThreadToken) s Text ()
firstGame params = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    threadToken  <- mapError' getThreadToken
    let game   = Game.Game
            { Game.player1          = pkh
            , Game.player2          = fpSecond params
            , Game.stake            = fpStake params
            , Game.playDeadline     = fpPlayDeadline params
            , Game.revealDeadline   = fpRevealDeadline params
            , Game.threadToken      = threadToken
            }
        client     = gameClient game
        stake      = lovelaceValueOf (fpStake params)
        nonce      = fpNonce params
        choice     = fpChoice params
        choiceBs   = GameChoice.toByteString choice
        choiceHash = sha2_256 $ nonce `concatenate` choiceBs

    void $ mapError' $ runInitialise client (State0 choiceHash) stake
    logInfo @String $ "first: made first move: " ++ show (fpChoice params)
    tell $ Last $ Just threadToken

    waitUntilTimeHasPassed $ fpPlayDeadline params

    m <- mapError' $ getOnChainState client
    case m of
        Nothing             -> throwError "first: game output not found"
        Just ((o, _), _) -> case tyTxOutData o of

            State0 _ -> do
                logInfo @String "first: player2 did not play"
                void $ mapError' $ runStep client GameRedeemer.Player1NoPlayClaim
                logInfo @String "first: player1 reclaimed stake"

            State1 _ player2Choice | GameChoice.beats choice player2Choice -> do
                logInfo @String "first: player2 played and lost"
                void $ mapError' $ runStep client $ GameRedeemer.Player1RevealWin nonce choice
                logInfo @String "first: player1 revealed and won"

            State1 _ player2Choice | choice == player2Choice -> do
                logInfo @String "first: player2 played and it is a draw"
                void $ mapError' $ runStep client $ GameRedeemer.Player1RevealDraw nonce choice
                logInfo @String "first: player1 revealed a draw"

            _ -> logInfo @String "first: second player played and won"

data Player2Params = Player2Params
    { spFirst          :: !PubKeyHash
    , spStake          :: !Integer
    , spPlayDeadline   :: !POSIXTime
    , spRevealDeadline :: !POSIXTime
    , spChoice         :: !GameChoice.GameChoice
    , spToken          :: !ThreadToken
    } deriving (Show, Generic, FromJSON, ToJSON)

secondGame :: forall w s. Player2Params -> Contract w s Text ()
secondGame params = do
    pkh <- pubKeyHash <$> Contract.ownPubKey
    let game   = Game.Game
            { Game.player1        = spFirst params
            , Game.player2        = pkh
            , Game.stake          = spStake params
            , Game.playDeadline   = spPlayDeadline params
            , Game.revealDeadline = spRevealDeadline params
            , Game.threadToken    = spToken params
            }
        choice = spChoice params
        client = gameClient game
    m <- mapError' $ getOnChainState client
    case m of
        Nothing          -> logInfo @String "second: no running game found"
        Just ((o, _), _) -> case tyTxOutData o of
            State0 _ -> do
                logInfo @String "second: running game found"
                void $ mapError' $ runStep client $ GameRedeemer.Player2Play choice
                logInfo @String $ "second: player2 played: " ++ show choice

                waitUntilTimeHasPassed $ spRevealDeadline params

                m' <- mapError' $ getOnChainState client
                case m' of
                    Nothing -> logInfo @String "second: first player won draw"
                    Just ((o, _), _) -> case tyTxOutData o of
                        State1 _ _ -> do
                            logInfo @String "second: player1 didn't reveal"
                            void $ mapError' $ runStep client GameRedeemer.Player2NoRevealClaim
                            logInfo @String "second: player2 won"
                        State2 _ _ -> do 
                            logInfo @String "second: player1 revealed a draw"
                            void $ mapError' $ runStep client GameRedeemer.Player2ClaimDraw
                            logInfo @String "second: draw"

                        _ -> throwError "second: unexpected datum after play"

            _ -> throwError "second: unexpected datum"

type GameSchema = Endpoint "first" Player1Params .\/ Endpoint "second" Player2Params

endpoints :: Contract (Last ThreadToken) GameSchema Text ()
endpoints = (first `select` second) >> endpoints
  where
    first  = endpoint @"first"  >>= firstGame
    second = endpoint @"second" >>= secondGame
