module Evaluate where

import Prelude (Ordering(EQ), compare, map, not, (<<<), (<=), (<>), ($), (==), (-), (/=), (&&), (>=), (||), (+))
import Data.Filterable (filterMap)
import Data.Foldable (fold)
import Data.Array (modifyAt, filter, mapWithIndex, sortBy, length, uncons, sort, reverse, fromFoldable, head, (:))
import Data.Tuple (Tuple(..))
import Data.Map as Map
import Data.Traversable (maximum, maximumBy, foldl, all)
import Control.Alt ((<|>))
import Control.Apply ((*>))
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Types (Hand, CommunityCards, Deck, Card(..), Color(..), Face(..), HandValue(..))
import Data.Enum (succ)

flush :: Hand -> HandValue
flush hand =
    let flushColor = maybe Spades (\(Card face color) -> color) $ head hand
        isFlush = (all (\(Card _ cardColor) -> cardColor == flushColor) hand)
                  &&
                  length hand == 5

    in
    if not isFlush
    then Unspecified
    else
        Flush $ reverse $ sort hand

straight :: Hand -> HandValue
straight hand =
    let sortedHand = sort hand
    in
    case uncons $ sortedHand of
        Just { head, tail} ->
            let firstFace =
                    case head of
                        Card face _ -> face
                result = 
                    foldl
                    (\{isStraight, lastFace} (Card face _) ->
                        let aceToFive = 
                                firstFace == Two && face == Ace && lastFace == Five
                        in
                        {isStraight:(isStraight && (succ lastFace == Just face || aceToFive)), lastFace:face} ) { isStraight:true, lastFace:((\(Card face _ ) -> firstFace) head)} tail
            in
            if result.isStraight && length sortedHand == 5
            then Straight $ reverse sortedHand
            else Unspecified
        Nothing -> Unspecified




pairsTripsQuads :: Hand -> HandValue
pairsTripsQuads hand = 
    let map1 =
            foldl
                (\stateMap (Card face color) ->
                    Map.alter
                        (\mMapValue ->
                            case mMapValue of 
                                Just arr -> Just $ (Card face color):arr
                                Nothing -> Just [(Card face color)]
                        )
                        face stateMap
                  ) Map.empty hand
        l = sortBy (\(Tuple k1 v1) (Tuple k2 v2) ->
                        let lengthCompared =  (length v1) `compare` (length v2)
                            keysCompared = k1 `compare` k2
                        in
                        if lengthCompared /= EQ
                        then lengthCompared
                        else keysCompared
                    ) $ fromFoldable $ Map.toList map1
        r = 
            foldl
            (\{pairsOrTrips, cards} (Tuple k v) ->
                let len = length v
                    newPairsOrTrips =
                        if len >= 2
                        then len:pairsOrTrips
                        else pairsOrTrips
                    newCards = v <> cards
                in
                { pairsOrTrips:newPairsOrTrips, cards:newCards }
            ) { pairsOrTrips:[], cards:[]} l
    in
        case r of
            { pairsOrTrips, cards } ->
                case sort pairsOrTrips of
                    [2] -> OnePair cards
                    [3] -> ThreeOfAKind cards
                    [2, 2] -> TwoPair cards
                    [2, 3] -> FullHouse cards
                    [4] -> FourOfAKind cards
                    _ -> HighCard cards
    


evaluate :: Hand -> HandValue
evaluate hand = 
    let s = straight hand
        f = flush hand
        p = pairsTripsQuads hand
    in
    if s /= Unspecified && f /= Unspecified
    then StraightFlush $ reverse $ sort hand
    else
        fromMaybe Unspecified $ maximum [s, f, p]


chooseCards :: Hand -> Int -> Array Hand
chooseCards chooseFrom chooseCount =
    let choose chooseFrom chosen chooseCount = 
            if chooseCount == 0
            then [chosen]
            else if chooseFrom == []
            then [[]]
            else
                case uncons chooseFrom of
                Nothing -> [[]]
                Just cf ->
                    let s = (choose cf.tail (cf.head:chosen) (chooseCount-1))
                    in
                        if chooseCount <= length cf.tail
                        then (choose cf.tail chosen chooseCount) <> s
                        else s
    in
    choose chooseFrom [] chooseCount

bestHand :: Hand -> HandValue
bestHand hand = 
    let allHands = chooseCards hand 5
        handValues = map evaluate allHands
    in
    fromMaybe (StraightFlush []) $  maximum handValues


getOuts1 :: Deck -> CommunityCards -> Hand -> Array Hand -> Array Card
getOuts1 deck communityCards heroHand villainHands = 
    let s = filterMap (\card ->
                let newCommunityCards = card:communityCards
                    best = maximumBy (\x y -> (bestHand (x<>newCommunityCards)) `compare` (bestHand (y<>newCommunityCards))) (heroHand:villainHands)
                in
                -- TODO programmed so that hero and villain can be equal
                if best == Just heroHand
                then Just card
                else Nothing
                ) deck
    in
    s


getOuts :: Deck -> CommunityCards -> Hand -> Array Hand -> Array Card
getOuts deck communityCards heroHand villainHands = 
    let cardsAndValues =
            filterMap
            (\card -> 
                let newCommunityCards = card:communityCards
                    newHeroValue = bestHand $ heroHand <> newCommunityCards
                    newVillainValue = fromMaybe (HighCard []) $ maximum $ map (\villainHand -> bestHand $ villainHand <> newCommunityCards) villainHands
                in
                    if newHeroValue <= newVillainValue
                    then Nothing
                    else Just {card:card, value:newHeroValue}
            ) deck
    in
    (map (\cardAndValue -> cardAndValue.card)
    <<<
    sortBy (\cardAndValue1 cardAndValue2 -> cardAndValue2.value `compare` cardAndValue1.value))
    cardsAndValues



getAllOuts :: Deck -> CommunityCards -> Array Hand -> Array (Array { card::Card, value::HandValue})
getAllOuts deck communityCards hands = 
    let cardsAndValues =
            foldl
            (\state card -> 
                let newCommunityCards = card:communityCards
                    handValues =
                        mapWithIndex
                        (\i hand -> 
                            { index: i, card: card, value: bestHand $ hand <> newCommunityCards }
                        )
                        hands
                    maxHandValue = fromMaybe { index: 14, card:card, value:HighCard [] } $ maximumBy (\x y -> x.value `compare` y.value) handValues
                in
                    fromMaybe state $ modifyAt maxHandValue.index (\x -> { card: maxHandValue.card,  value: maxHandValue.value}:x) state
            )
            (map (\_ -> []) hands)
            deck
    in
    cardsAndValues
    --(map (\cardAndValue -> cardAndValue.card)
    -- <<<
    --sortBy (\cardAndValue1 cardAndValue2 -> cardAndValue2.value `compare` cardAndValue1.value))
    --cardsAndValues







