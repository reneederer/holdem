module Deal where

import Prelude (map, pure, bind, ($), (-), (<>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Data.Array ( uncons, snoc, foldM, deleteAt, length, (..), (:), (!!))
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Enum (enumFromTo)
import Types (Deal, Deck, Hand, Card(..), Color(..), Face(..))

fullDeck :: Array Card
fullDeck = do
    a <- enumFromTo Two Ace
    b <- [Spades, Clubs, Diamonds, Hearts]
    pure $ Card a b

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

dealHandsFromDeck :: forall e. Deck -> Array Int -> Eff (random :: RANDOM | e) { hands:: Array Hand, deck::Deck }
dealHandsFromDeck deck cardCountArr =
    foldM (\state cardCount -> do
        deal <- dealHand state.deck cardCount
        pure {hands:(snoc state.hands deal.hand), deck:deal.deck}
        ) {hands:[], deck:deck} cardCountArr


dealHands :: forall e. Int -> Int -> Eff (random :: RANDOM | e) Deal
dealHands communityCardCount playerCount = do
    deal <- dealHandsFromDeck fullDeck $ [communityCardCount] <> (map (\_ -> 2) (1 .. playerCount))
    case uncons deal.hands of
        Just d -> 
            pure $ { communityCards:d.head, hands: d.tail }
        Nothing ->
            pure $ { communityCards: [], hands: [] }











