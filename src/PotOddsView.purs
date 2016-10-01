module PotOddsView where

import Prelude (flip, type (~>), class Show, pure, bind, show, map, (<>), ($), (+), mod, (==), (||), (-), (/), (*), (<=), (/=), negate, max)
import Data.Array (concatMap, uncons, mapWithIndex, concat, (\\), (..), (:))
import Data.Generic (class Generic, gEq, gShow)
import Data.String (toLower, length, takeWhile, drop, indexOf)
import Math (pow, round)
import Data.Int (floor)
import Data.Int (toNumber)
import Control.Alt ((<|>))
import Control.Apply ((*>))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Data.Functor (class Functor)
import Control.Monad.Eff.Random (RANDOM, randomInt, random, randomRange)
import Data.Maybe (Maybe(Nothing, Just), fromMaybe)
import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as PI
import Types (Hand, Card(..))
import Evaluate (bestHand, getOuts, getAllOuts)
import Deal (fullDeck, dealHands)

type State =
    { pot:: Number
    , bet:: Number
    , communityCards:: Hand
    , playerHands:: Array Hand
    }

data Query a = 
    NewPotOddsScenario a

initialState :: forall e. Eff (random :: RANDOM | e) State
initialState = createPotOddsScenario


createPotOddsScenario :: forall e. Eff (random :: RANDOM | e) State
createPotOddsScenario =
    pure
        { pot: 128.8
        , bet: 23.2
        , communityCards: []
        , playerHands: [[]]
        }

ui :: forall eff. H.Component State Query (Aff (random :: RANDOM | eff))
ui =
    H.component { render, eval }
    where
    render :: State -> H.ComponentHTML Query
    render state =
        HH.div_ $
            [ HH.div_ $
                [ HH.text "hallo"
                , HH.br_
                , HH.br_
                ]
                <>
                (concatMap
                    (\e ->
                        [ HH.text $ show e
                        , HH.br_
                        ]
                    ) ["a","b","c"]
                )
                <>
                [ HH.div_
                    [ HH.button
                        [ HE.onClick (HE.input_ NewPotOddsScenario) ]
                        [HH.text "Next" ]
                    ]
                ]
            ]

    eval :: Query ~> H.ComponentDSL State Query (Aff (random :: RANDOM | eff))
    eval (NewPotOddsScenario next) = do
        potOddsScenario <- H.fromEff createPotOddsScenario
        H.modify (\state -> potOddsScenario)
        pure next


