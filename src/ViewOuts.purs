module ViewOuts where

import Prelude (type (~>), pure, bind, show, map, (<>), ($), (+))
import Data.Array (uncons, mapWithIndex, concat, (\\), (..), (:))
import Data.String (toLower)
import Control.Alt ((<|>))
import Control.Apply ((*>))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Data.Functor (class Functor)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Data.Maybe (Maybe(Nothing, Just))
import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as PI
import Types (Hand, Card(..))
import Evaluate (bestHand, getOuts, getAllOuts)
import Deal (fullDeck, dealHands)


type State =
    { hands::Array Hand
    , communityCards::Array Card }

initialState :: forall e. Eff (random :: RANDOM | e) State
initialState =
    randomCards 3 2

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
                        --outs = getOuts deck state.communityCards heroAndVillains.head heroAndVillains.tail
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
        deal <- H.fromEff $ randomCards communityCardCount playerCardCount
        H.modify (\state -> deal)
        pure next

handToImgs ::  forall a b c. (Functor a) => a Card -> a (HH.HTML b c)
handToImgs hand =
    map cardToImg hand

cardToImg :: forall a b. Card -> HH.HTML a b
cardToImg card = 
    HH.img
        [ PI.src $ getImageFile card
        , PI.alt $ getImageFile card
        , PI.width $ PI.Pixels 80 ]


getImageFile :: Card -> String
getImageFile (Card face color) = 
    let faceStr = (toLower $ show face)
        colorStr = (toLower $ show color)
    in
    "imgs/" <> faceStr <> "_of_" <> colorStr <> ".svg"

randomCards :: forall e. Int -> Int -> Eff (random :: RANDOM | e) { hands::Array Hand, communityCards :: Array Card }
randomCards communityCardCount playerCount = do
    deal <- dealHands fullDeck $ [communityCardCount] <> (map (\_ -> 2) (1 .. playerCount))
    let newCards =
            case uncons deal.hands of
                Just d -> 
                    { hands:d.tail, communityCards:d.head }
                Nothing ->
                    { hands:[], communityCards:[] }
    pure newCards















