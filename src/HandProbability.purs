module HandProbability where

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
import Types (Deal, Query(..), HandProbabilityState(..))
import CSS.Geometry as CSS
import CSS.Size as CSS
import CSS.Background as CSS
import CSS.Color as CSS
import Data.Foreign.Class (readProp)
import Data.Either (Either(..))


createPuzzle:: forall e. Eff (random :: RANDOM | e) HandProbabilityState
createPuzzle = do
    communityCardCount <- randomInt 3 4
    playerCount <- randomInt 2 3
    deal <- dealHands communityCardCount playerCount
    pure $
        { deal: deal
        , showSolution: false
        }

render :: HandProbabilityState -> H.ComponentHTML Query
render handProbabilityState =
    HH.div_ $
        [ HH.div_ $
            [ HH.text "HandProbability"
            , HH.text $ "Community Cards: " <> show handProbabilityState.deal.communityCards
            , HH.br_
            , HH.br_
            ] <>
            (handToImgs 80 handProbabilityState.deal.communityCards)
            <>
            [ HH.br_
            , HH.br_
            ,
                let deck = fullDeck \\ handProbabilityState.deal.communityCards \\ (concat handProbabilityState.deal.hands)
                    outsPerPlayer = getAllOuts deck handProbabilityState.deal
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
                let deck = fullDeck \\ handProbabilityState.deal.communityCards \\ (concat handProbabilityState.deal.hands)
                    outsPerPlayer = getAllOuts deck handProbabilityState.deal
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
                    , HH.text $ show $ bestHand $ handProbabilityState.deal.communityCards <> hand
                    , HH.br_
                    , HH.br_
                    ] <>
                    (handToImgs 80 hand)
                    )

            ) handProbabilityState.deal.hands
        )
        <>
         [ HH.div_
            [ HH.button
                [ HE.onClick (HE.input_ CreateRandomPuzzle) ]
                [ HH.text "New Puzzle" ]
            ]
        ]








