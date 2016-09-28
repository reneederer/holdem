module Main  where

import Prelude
import Data.Enum
import Data.Array
import Data.String (drop, joinWith)
import Data.Tuple
import Data.Map as Map
import Data.Foldable
import Debug.Trace
import Data.Traversable
import Data.Generic
import Data.Maybe
import Data.Functor
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random
import Control.Monad.Eff.Console (CONSOLE, log)

data Color = 
      Diamonds
    | Spades
    | Hearts
    | Clubs
derive instance genericColor :: Generic Color
instance eqColor :: Eq Color where
    eq = gEq
instance showColor :: Show Color where
    show x = drop 5 $ gShow x


data Face = 
      Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace
derive instance genericFace :: Generic Face
instance eqFace :: Eq Face where
    eq x z = gEq x z
instance showFace :: Show Face where
    show x = drop 5 $ gShow x
instance ordFace :: Ord Face where
    compare face1 face2 =
        if face1 == face2 then EQ
        else
            case {f1:face1, f2:face2} of
            {f1:Ace, f2:_} -> GT
            {f1:_, f2:Ace} -> LT
            {f1:King, f2:_} -> GT
            {f1:_, f2:King} -> LT
            {f1:Queen, f2:_} -> GT
            {f1:_, f2:Queen} -> LT
            {f1:Jack, f2:_} -> GT
            {f1:_, f2:Jack} -> LT
            {f1:Ten, f2:_} -> GT
            {f1:_, f2:Ten} -> LT
            {f1:Nine, f2:_} -> GT
            {f1:_, f2:Nine} -> LT
            {f1:Eight, f2:_} -> GT
            {f1:_, f2:Eight} -> LT
            {f1:Seven, f2:_} -> GT
            {f1:_, f2:Seven} -> LT
            {f1:Six, f2:_} -> GT
            {f1:_, f2:Six} -> LT
            {f1:Five, f2:_} -> GT
            {f1:_, f2:Five} -> LT
            {f1:Four, f2:_} -> GT
            {f1:_, f2:Four} -> LT
            {f1:Three, f2:_} -> GT
            {f1:_, f2:Three} -> LT
            {f1:_, f2:_} -> EQ
instance enumFace :: Enum Face where
    succ Two = Just Three
    succ Three = Just Four
    succ Four = Just Five
    succ Five = Just Six
    succ Six = Just Seven
    succ Seven = Just Eight
    succ Eight = Just Nine
    succ Nine = Just Ten
    succ Ten = Just Jack
    succ Jack = Just Queen
    succ Queen = Just King
    succ King = Just Ace
    succ Ace = Nothing

    pred Two = Nothing
    pred Three = Just Two
    pred Four = Just Three
    pred Five = Just Four
    pred Six = Just Five
    pred Seven = Just Six
    pred Eight = Just Seven
    pred Nine = Just Eight 
    pred Ten = Just Nine 
    pred Jack = Just Ten 
    pred Queen = Just Jack 
    pred King = Just Queen 
    pred Ace = Just King 

data HandValue = 
      StraightFlush (Array Card)
    | FourOfAKind (Array Card)
    | FullHouse (Array Card)
    | Flush (Array Card)
    | Straight (Array Card)
    | ThreeOfAKind (Array Card)
    | TwoPair (Array Card)
    | OnePair (Array Card)
    | HighCard (Array Card)
    | Unspecified
derive instance genericHandValue :: Generic HandValue
instance eqHandValue :: Eq HandValue where
    eq x z = gEq x z
instance showHandValue :: Show HandValue where
    show x =
        case x of
          StraightFlush cards -> "Straight Flush: " <> (joinWith ", " <<< map show) cards
          FourOfAKind cards -> "Four Of A Kind: " <> (joinWith ", " <<< map show) cards
          FullHouse cards -> "Full House: " <> (joinWith ", " <<< map show) cards
          Flush cards -> "Flush: " <> (joinWith ", " <<< map show) cards
          Straight cards -> "Straight: " <> (joinWith ", " <<< map show) cards
          ThreeOfAKind cards -> "Three Of A Kind: " <> (joinWith ", " <<< map show) cards
          TwoPair cards -> "Two Pair: " <> (joinWith ", " <<< map show) cards
          OnePair cards -> "One Pair: " <> (joinWith ", " <<< map show) cards
          HighCard cards -> "High Card: " <> (joinWith ", " <<< map show) cards
          Unspecified -> "Unspecified"
instance ordHandValue :: Ord HandValue where
    compare h1 h2 =
        case { h1:h1, h2:h2 } of
            { h1:StraightFlush c1, h2:StraightFlush c2 } -> c1 `compare` c2
            { h1:StraightFlush _, h2:_ } -> GT
            { h1:_, h2:StraightFlush _ } -> LT
            { h1:FourOfAKind c1, h2:FourOfAKind c2 } -> c1 `compare` c2
            { h1:FourOfAKind _, h2:_ } -> GT
            { h1:_, h2:FourOfAKind _ } -> LT
            { h1:FullHouse c1, h2:FullHouse c2 } -> c1 `compare` c2
            { h1:FullHouse _, h2:_ } -> GT
            { h1:_, h2:FullHouse _ } -> LT
            { h1:Flush c1, h2:Flush c2 } -> c1 `compare` c2
            { h1:Flush _, h2:_ } -> GT
            { h1:_, h2:Flush _ } -> LT
            { h1:Straight c1, h2:Straight c2 } -> c1 `compare` c2
            { h1:Straight _, h2:_ } -> GT
            { h1:_, h2:Straight _ } -> LT
            { h1:ThreeOfAKind c1, h2:ThreeOfAKind c2 } -> c1 `compare` c2
            { h1:ThreeOfAKind _, h2:_ } -> GT
            { h1:_, h2:ThreeOfAKind _ } -> LT
            { h1:TwoPair c1, h2:TwoPair c2 } -> c1 `compare` c2
            { h1:TwoPair _, h2:_ } -> GT
            { h1:_, h2:TwoPair _ } -> LT
            { h1:OnePair c1, h2:OnePair c2 } -> c1 `compare` c2
            { h1:OnePair _, h2:_ } -> GT
            { h1:_, h2:OnePair _ } -> LT
            { h1:HighCard c1, h2:HighCard c2 } -> c1 `compare` c2
            { h1:HighCard _, h2:_ } -> GT
            { h1:_, h2:HighCard _ } -> LT
            _ -> EQ

data Card = Card Face Color
derive instance genericCard :: Generic Card
instance eqCard :: Eq Card where
    eq x z = gEq x z
instance showCard :: Show Card where
    show (Card face color) = (show face) <> " of " <> (show color)
instance ordCard :: Ord Card where
    compare (Card face1 color1) (Card face2 color2) =
        face1 `compare` face2
       

type Hand = Array Card 
type Deck = Array Card 
type PlayerCards = Array Card 
type CommunityCards = Array Card 


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

--main :: forall e. Eff (random :: RANDOM, console :: CONSOLE | e) Unit
main = do
    deal <- (deal [5, 2, 2])
    case uncons deal.hands of
        Just hands -> do
            let x = map (evaluate <<< ((<>) hands.head)) hands.tail
            log $ "Community: " <> show hands.head
            foldM (\state x -> do
                log $ show x
                log $ show $ bestHand (x <> hands.head)
                pure state) unit hands.tail
        Nothing -> log "hallo"

    
getOuts :: Deck -> CommunityCards -> Hand -> Hand -> Array Card
getOuts deck communityCards hero villain = 
    filter
    (\card -> 
        let newCommunityCards = card:communityCards
            newHeroValue = bestHand $ hero <> newCommunityCards
            newVillainValue = bestHand $ villain <> newCommunityCards
        in
            newHeroValue >= newVillainValue
    ) deck



fullDeck = do
    a <- enumFromTo Two Ace
    b <- [Spades, Clubs, Diamonds, Hearts]
    pure $ Card a b

deal = dealHands fullDeck

dealCard :: forall e. Deck -> Eff (random :: RANDOM | e) {deck::Deck, card::Card}
dealCard deck = do
    r <- randomInt 0 (length deck - 1)
    let card = fromMaybe (Card Two Spades) $ deck !! r
    let newDeck = fromMaybe [] $ deleteAt r deck 
    pure {deck:newDeck, card:card}


dealHand :: forall e. Deck -> Int -> Eff (random :: RANDOM | e) { hand::Hand, deck::Deck }
dealHand deck cardCount = 
    foldM (\state _ -> do
        deal <- dealCard state.deck
        pure {hand:(deal.card:state.hand), deck:deal.deck}
        ) {hand:[], deck:deck} (1 .. cardCount)

dealHands :: forall e. Deck -> Array Int -> Eff (random :: RANDOM | e) { hands:: Array Hand, deck::Deck }
dealHands deck cardCountArr =
    foldM (\state cardCount -> do
        deal <- dealHand state.deck cardCount
        pure {hands:(snoc state.hands deal.hand), deck:deal.deck}
        ) {hands:[], deck:deck} cardCountArr

    

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


bestHand hand = 
    let allHands = chooseCards hand 5
        handValues = map evaluate allHands
    in
    fromMaybe (StraightFlush []) $  maximum handValues
    








