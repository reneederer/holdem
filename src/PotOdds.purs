module PotOdds where

import Prelude (type (~>), pure, bind, show, map, (<>), ($), (+), ($>), (/), (*))
import Data.Int (toNumber, floor)
import Control.Alt ((<|>))
import Control.Apply ((*>))
import Control.Monad.Aff (Aff)
import Data.Foldable (sum)
import Data.Maybe (Maybe(Nothing, Just))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, randomInt, randomRange)
import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Evaluate (bestHand, getAllOuts)
import Deal (fullDeck, dealHands)
import ViewHelperFunctions
import Data.Array (uncons, length, mapWithIndex, concat, (!!), (\\))
import Data.Maybe (fromMaybe)
import Halogen.HTML.Properties.Indexed as PI
import Halogen.HTML.CSS.Indexed as CSS
import Types (Deal, Query(..), PotOddsState(..))
import CSS.Geometry as CSS
import CSS.Size as CSS
import CSS.Background as CSS
import CSS.Color as CSS
import Data.Foreign.Class (readProp)
import Data.Either (Either(..))


createPuzzle:: forall e. Eff (random :: RANDOM | e) PotOddsState
createPuzzle = do
    communityCardCount <- randomInt 3 4
    playerCount <- randomInt 2 3
    pot <-  randomInt 10 200
    b <- randomRange 0.1 3.0
    let bet = floor $ (toNumber pot) * b
    deal <- dealHands communityCardCount playerCount
    pure $
        { pot: pot
        , bet: bet
        , deal: deal
        , showSolution: false
        }

render :: PotOddsState -> H.ComponentHTML Query
render potOddsState =
    HH.div_ $
        [ HH.div_ $
            [ HH.text "PotOdds"
            , HH.text $ "Community Cards: " <> show potOddsState.deal.communityCards
            , HH.br_
            , HH.br_
            ] <>
            (handToImgs 80 potOddsState.deal.communityCards)
            <>
            [ HH.br_
            , HH.br_
            , HH.text $ "Pot size: " <> show potOddsState.pot
            , HH.br_
            , HH.text $ "To call: " <> show potOddsState.bet
            , HH.br_
            ,
                let deck = fullDeck \\ potOddsState.deal.communityCards \\ (concat potOddsState.deal.hands)
                    outsPerPlayer = getAllOuts deck potOddsState.deal
                    outCount =
                        case uncons outsPerPlayer of
                        Just o ->
                            let hero = length o.head
                                villains = sum $ map length outsPerPlayer
                            in
                            { hero: hero, villains: villains }
                        Nothing -> { hero: 0, villains: 0 }
                in
                HH.div_
                    [ HH.text $ "Probability: " <> (show $ (toNumber outCount.hero) / (toNumber $ outCount.hero + outCount.villains))
                    , HH.text $ "Expectation value: " <> show outCount.hero
                    ]
            ,
                let deck = fullDeck \\ potOddsState.deal.communityCards \\ (concat potOddsState.deal.hands)
                    outsPerPlayer = getAllOuts deck potOddsState.deal
                in
                HH.div_ $
                [ HH.text $ "Outs: " <> (show $ map (\player -> (map (\out -> out.card) player)) outsPerPlayer) ]
            ]

        ] <> 
        (mapWithIndex
            (\i hand ->
                HH.div_
                    ([ HH.text $ "Player " <> show (i + 1) <> ": " <> show hand
                    , HH.br_
                    , HH.text $ show $ bestHand $ potOddsState.deal.communityCards <> hand
                    , HH.br_
                    , HH.br_
                    ] <>
                    (handToImgs 80 hand)
                    )

            ) potOddsState.deal.hands
        )
        <>
         [ HH.div_
            [ HH.button
                [ HE.onClick (HE.input_ CreateRandomPuzzle) ]
                [ HH.text "New Puzzle" ]
            ]
        ]








