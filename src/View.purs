module View where

import Prelude (type (~>), pure, bind, show, map, (<>), ($), (+), ($>))
import Control.Alt ((<|>))
import Control.Apply ((*>), (<$>))
import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe(Nothing, Just))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Evaluate (bestHand, getAllOuts)
import Deal (fullDeck, dealHands)
import ViewHelperFunctions
import Data.Array (length, mapWithIndex, concat, (!!), (\\))
import Data.Maybe (fromMaybe)
import Halogen.HTML.Properties.Indexed as PI
import Halogen.HTML.CSS.Indexed as CSS
import Types (Deal, Query(..), PotOddsState, OutsState, ConvertNumberState, HandProbabilityState)
import CSS.Geometry as CSS
import CSS.Size as CSS
import CSS.Background as CSS
import CSS.Color as CSS
import Data.Foreign.Class (readProp)
import Data.Either (Either(..))
import PotOdds as PotOdds
import Outs as Outs
import ConvertNumber as ConvertNumber
import HandProbability as HandProbability

data State =
      OutsPuzzle OutsState
    | PotOddsPuzzle PotOddsState
    | ConvertNumberPuzzle ConvertNumberState
    | HandProbabilityPuzzle HandProbabilityState

initialState :: forall e. Eff (random :: RANDOM | e) State
initialState = do
    ConvertNumberPuzzle <$> ConvertNumber.createPuzzle



ui :: forall eff. H.Component State Query (Aff (random :: RANDOM | eff))
ui =
    H.component { render, eval }
    where
    render :: State -> H.ComponentHTML Query
    render state =
        case state of
            OutsPuzzle s -> Outs.render s
            PotOddsPuzzle s -> PotOdds.render s
            ConvertNumberPuzzle s -> ConvertNumber.render s
            HandProbabilityPuzzle s -> HandProbability.render s
            

    eval :: Query ~> H.ComponentDSL State Query (Aff (random :: RANDOM | eff))
    eval (CreateRandomPuzzle next) = do
        r <- H.fromEff $ randomInt 1 4
        case r of
            1 -> eval $ CreateOutsPuzzle next
            2 -> eval $ CreateConvertNumberPuzzle next
            3 -> eval $ CreatePotOddsPuzzle next
            _ -> eval $ CreateHandProbabilityPuzzle next
    eval (CreateOutsPuzzle next) = do
        outsPuzzle <- H.fromEff $ Outs.createPuzzle
        H.modify (\state -> OutsPuzzle outsPuzzle)
        pure next
    eval (CreateConvertNumberPuzzle next) = do
        convertNumberPuzzle <- H.fromEff $ ConvertNumber.createPuzzle
        H.modify (\state ->  ConvertNumberPuzzle convertNumberPuzzle)
        pure next
    eval (CreatePotOddsPuzzle next) = do
        potOddsPuzzle <- H.fromEff $ PotOdds.createPuzzle
        H.modify (\state -> PotOddsPuzzle potOddsPuzzle)
        pure next
    eval (CreateHandProbabilityPuzzle next) = do
        handProbabilityPuzzle <- H.fromEff $ HandProbability.createPuzzle
        H.modify (\state -> HandProbabilityPuzzle handProbabilityPuzzle)
        pure next
    eval (ShowSolution next) = do
        H.modify
            (\state ->
                case state of
                    OutsPuzzle s -> OutsPuzzle $ s { showSolution=true }
                    PotOddsPuzzle s -> PotOddsPuzzle $ s { showSolution=true }
                    HandProbabilityPuzzle s -> HandProbabilityPuzzle $ s { showSolution=true }
                    ConvertNumberPuzzle s -> ConvertNumberPuzzle $ s { showSolution=true }
            )
        pure next














