module Outs where

import Prelude (type (~>), pure, bind, show, map, (<>), ($), (+), ($>))
import Control.Alt ((<|>))
import Control.Apply ((*>))
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
import Types (Deal, Query(..), OutsState(..))
import CSS.Geometry as CSS
import CSS.Size as CSS
import CSS.Background as CSS
import CSS.Color as CSS
import Data.Foreign.Class (readProp)
import Data.Either (Either(..))


createPuzzle :: forall e. Eff (random :: RANDOM | e) OutsState
createPuzzle = do
    communityCardCount <- randomInt 3 4
    playerCount <- randomInt 2 3
    deal <- dealHands communityCardCount playerCount
    pure
        { deal: deal
        , showSolution: false
        }

render :: OutsState -> H.ComponentHTML Query
render outsState =
    let deck = fullDeck \\ outsState.deal.communityCards \\ (concat outsState.deal.hands)
        outsPerPlayer = getAllOuts deck outsState.deal
        outCardsPerPlayer = map (\player -> (map (\out -> out.card) player)) outsPerPlayer
    in
    HH.div_
        $
        [ HH.div_ $
            [ HH.text "Outs"
            , HH.text $ "Community Cards: " <> show outsState.deal.communityCards
            , HH.br_
            , HH.br_
            ] <>
            (handToImgs 60 outsState.deal.communityCards)
            <>
            [ HH.br_
            , HH.br_
            ]

        ] <> 
        (mapWithIndex
            (\i hand ->
                let outCards = fromMaybe [] (outCardsPerPlayer !! i)
                    outCount = length outCards
                in
                HH.div_
                    ([ HH.text $ "Player " <> show (i + 1) <> ": " <> show hand
                    , HH.br_
                    , HH.text $ show $ bestHand $ outsState.deal.communityCards <> hand
                    , HH.br_
                    , HH.br_
                    ]
                    <>
                    (handToImgs 60 hand)
                    <>
                    [ HH.br_
                    , HH.br_
                    , HH.text $ "Out count: " <> (show outCount)
                    , HH.br_
                    , HH.text $ "Outs: "
                    , HH.br_
                    , HH.br_
                    ] <>
                    (handToImgs 50 outCards)
                    <>
                    [ HH.br_
                    , HH.br_
                    , HH.br_
                    , HH.br_
                    ]
                    )

            ) outsState.deal.hands
        )
        <>
         [ HH.div_
            [ HH.button
                [ HE.onClick (HE.input_ CreateRandomPuzzle) ]
                [ HH.text "New Puzzle" ]
            ]
        ]

