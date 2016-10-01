module ViewOuts where

import Prelude (type (~>), pure, bind, show, map, (<>), ($), (+))
import Data.Array (uncons, mapWithIndex, concat, (\\), (..), (:))
import Control.Alt ((<|>))
import Control.Apply ((*>))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Data.Maybe (Maybe(Nothing, Just))
import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Types (Hand, Card)
import Evaluate (bestHand, getAllOuts)
import Deal (fullDeck, dealHands)
import ViewHelperFunctions


type State =
    { hands::Array Hand
    , communityCards::Array Card }

initialState :: forall e. Eff (random :: RANDOM | e) State
initialState =
    dealHands 3 2

data Query a =
      NewCards a

ui :: forall eff. H.Component State Query (Aff (random :: RANDOM | eff))
ui =
    H.component { render, eval }
    where
    render :: State -> H.ComponentHTML Query
    render state =
        HH.div_ $
            [ HH.div_ $
                [ HH.text $ "Community Cards: " <> show state.communityCards
                , HH.br_
                , HH.br_
                ] <>
                (handToImgs state.communityCards)
                <>
                [ HH.br_
                , HH.br_
                ,
                    let deck = fullDeck \\ state.communityCards \\ (concat state.hands)
                        heroAndVillains = 
                            case uncons state.hands of
                                Just hands -> hands
                                Nothing -> {head: [], tail: [] }
                        outsPerPlayer = getAllOuts deck state.communityCards (heroAndVillains.head:heroAndVillains.tail)
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
                        , HH.text $ show $ bestHand $ state.communityCards <> hand
                        , HH.br_
                        , HH.br_
                        ] <>
                        (handToImgs hand)
                        )

                ) state.hands
            )
            <>
            [ HH.div_
                [ HH.button
                    [ HE.onClick (HE.input_ NewCards) ]
                    [ HH.text "Off" ]
                ]
            ]

    eval :: Query ~> H.ComponentDSL State Query (Aff (random :: RANDOM | eff))
    eval (NewCards next) = do
        communityCardCount <- H.fromEff $ randomInt 3 4
        playerCardCount <- H.fromEff $ randomInt 2 3
        deal <- H.fromEff $ dealHands communityCardCount playerCardCount
        H.modify (\state -> deal)
        pure next

















