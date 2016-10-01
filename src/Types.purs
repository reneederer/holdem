module Types where

import Prelude (class Ord, class Show, class Eq, Ordering(..), compare, show, map, negate, (<>), ($), (<<<), (==), (+))
import Data.Generic (class Generic, gEq, gShow)
import Data.Enum (class Enum)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith, lastIndexOf, drop)

data Color = 
      Diamonds
    | Spades
    | Hearts
    | Clubs
derive instance genericColor :: Generic Color
instance eqColor :: Eq Color where
    eq = gEq
instance showColor :: Show Color where
    show color =
        let colorStr = gShow color
        in
        drop (1 + (fromMaybe 0 $ lastIndexOf "." colorStr)) colorStr

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
    show face =
        let faceStr = gShow face
        in
        drop (1 + (fromMaybe (-1) $ lastIndexOf "." faceStr)) faceStr
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
instance enumHandValue :: Enum HandValue where
    succ (HighCard _) = Just $ OnePair []
    succ (OnePair _) = Just $ TwoPair []
    succ (TwoPair _) = Just $ ThreeOfAKind []
    succ (ThreeOfAKind _) = Just $ Straight []
    succ (Straight _) = Just $ Flush []
    succ (Flush _) = Just $ FourOfAKind []
    succ (FourOfAKind _) = Just $ StraightFlush []
    succ _ = Nothing

    pred (OnePair _) = Just $ HighCard []
    pred (TwoPair _) = Just $ OnePair []
    pred (ThreeOfAKind _) = Just $ TwoPair []
    pred (Straight _) = Just $ ThreeOfAKind []
    pred (Flush _) = Just $ Straight []
    pred (FourOfAKind _) = Just $ Flush []
    pred (StraightFlush _) = Just $ FourOfAKind []
    pred _ = Nothing

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















