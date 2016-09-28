module Test.Main where

import Prelude
import Data.Array
import Test.Unit
import Test.Unit.Main
import Test.Unit.Assert as Assert
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Main 
import Data.Maybe


main = runTest do
    suite "Find all Outs" do
        let communityCards = 
                [ Card Nine Clubs
                , Card Five Diamonds
                , Card Eight Spades
                ]
        let heroCards = 
                [ Card Seven Spades
                , Card Six Clubs
                ]
        let villainCards = 
                [ Card Nine Hearts
                , Card Nine Spades
                ]
        let deck = fullDeck \\ communityCards \\ heroCards \\ villainCards
        test "Find the outs" do
            Assert.equal
                [ Card Seven Hearts
                , Card Seven Diamonds
                ] $ 
                getOuts deck communityCards heroCards villainCards
    suite "Find best hand out of 7 cards" do
        test "RoyalFlush" do
            Assert.equal
                (StraightFlush
                    [ Card Ace Hearts
                    , Card King Hearts
                    , Card Queen Hearts
                    , Card Jack Hearts
                    , Card Ten Hearts
                    ]
                ) $
                bestHand
                    [ Card Ace Hearts
                    , Card Seven Diamonds
                    , Card Three Clubs
                    , Card King Hearts
                    , Card Ten Hearts
                    , Card Queen Hearts
                    , Card Jack Hearts
                    ]
        test "A Straight cannot go round" do
            Assert.equal
                (HighCard
                    [ Card Ace Hearts
                    , Card King Spades
                    , Card Queen Spades
                    , Card Seven Diamonds
                    , Card Six Clubs
                    ]
                ) $
                bestHand
                    [ Card Ace Hearts
                    , Card Seven Diamonds
                    , Card Three Clubs
                    , Card King Spades
                    , Card Two Spades
                    , Card Queen Spades
                    , Card Six Clubs
                    ]
    suite "Recognize 5 cards" do
        test "A Straight cannot go round, it is a HighCard" do
            Assert.equal
                (HighCard
                    [ Card Ace Hearts
                    , Card King Spades
                    , Card Queen Spades
                    , Card Three Clubs
                    , Card Two Spades
                    ]
                ) $
                evaluate
                    [ Card Queen Spades
                    , Card King Spades
                    , Card Ace Hearts
                    , Card Two Spades
                    , Card Three Clubs
                    ]
        test "High card" do
            Assert.equal
                (HighCard
                    [ Card Ace Diamonds
                    , Card Jack Clubs
                    , Card Ten Spades
                    , Card Seven Hearts
                    , Card Two Clubs
                    ]
                ) $
                evaluate
                    [ Card Ace Diamonds
                    , Card Seven Hearts
                    , Card Ten Spades
                    , Card Two Clubs
                    , Card Jack Clubs
                    ]
        test "OnePair is recognized" do
            Assert.equal
                (OnePair
                    [ Card Seven Clubs
                    , Card Seven Hearts
                    , Card Ace Diamonds
                    , Card Jack Clubs
                    , Card Ten Spades
                    ]
                ) $
                evaluate
                    [ Card Ace Diamonds
                    , Card Seven Hearts
                    , Card Ten Spades
                    , Card Seven Clubs
                    , Card Jack Clubs
                    ]
        test "TwoPair are recognized" do
            Assert.equal
                (TwoPair
                    [ Card Jack Clubs
                    , Card Jack Diamonds
                    , Card Seven Clubs
                    , Card Seven Hearts
                    , Card Ten Spades
                    ]
                ) $
                evaluate
                    [ Card Jack Diamonds
                    , Card Seven Hearts
                    , Card Ten Spades
                    , Card Seven Clubs
                    , Card Jack Clubs
                    ]
        test "ThreeOfAKind are recognized" do
            Assert.equal
                (ThreeOfAKind
                    [ Card Jack Clubs
                    , Card Jack Hearts
                    , Card Jack Diamonds
                    , Card Ten Spades
                    , Card Seven Hearts
                    ]
                ) $
                evaluate
                    [ Card Jack Diamonds
                    , Card Seven Hearts
                    , Card Ten Spades
                    , Card Jack Hearts
                    , Card Jack Clubs
                    ]
        test "A Fullhouse is recognized" do
            Assert.equal
                (FullHouse
                    [ Card Jack Clubs
                    , Card Jack Spades
                    , Card Jack Diamonds
                    , Card Eight Clubs
                    , Card Eight Hearts
                    ]
                ) $
                evaluate
                    [ Card Jack Diamonds
                    , Card Jack Spades
                    , Card Eight Hearts
                    , Card Eight Clubs
                    , Card Jack Clubs
                    ]
        test "Four of a kind are recognized" do
            Assert.equal
                (FourOfAKind
                    [ Card Jack Clubs
                    , Card Jack Hearts
                    , Card Jack Spades
                    , Card Jack Diamonds
                    , Card Eight Clubs
                    ]
                ) $
                evaluate
                    [ Card Jack Diamonds
                    , Card Jack Spades
                    , Card Jack Hearts
                    , Card Eight Clubs
                    , Card Jack Clubs
                    ]
        test "A straight does not need to be in order" do
            Assert.equal
                (Straight
                    [ Card Jack Diamonds
                    , Card Ten Spades
                    , Card Nine Hearts
                    , Card Eight Clubs
                    , Card Seven Hearts
                    ]
                ) $
                evaluate
                    [ Card Seven Hearts
                    , Card Ten Spades
                    , Card Jack Diamonds
                    , Card Eight Clubs
                    , Card Nine Hearts
                    ]
        test "A straight can go from Ace to Five" do
            Assert.equal
                (Straight
                    [ Card Ace Diamonds
                    , Card Five Hearts
                    , Card Four Clubs
                    , Card Three Hearts
                    , Card Two Spades
                    ]
                ) $
                evaluate
                    [ Card Three Hearts
                    , Card Two Spades
                    , Card Five Hearts
                    , Card Ace Diamonds
                    , Card Four Clubs
                    ]
        test "Flushes are recognized" do
            Assert.equal
                (Flush
                    [ Card King Spades
                    , Card Jack Spades
                    , Card Ten Spades
                    , Card Seven Spades
                    , Card Two Spades
                    ]
                ) $
                evaluate
                    [ Card King Spades
                    , Card Seven Spades
                    , Card Two Spades
                    , Card Jack Spades
                    , Card Ten Spades
                    ]
        test "StraightFlush is recognized" do
            Assert.equal
                (StraightFlush
                    [ Card Ace Diamonds
                    , Card King Diamonds
                    , Card Queen Diamonds
                    , Card Jack Diamonds
                    , Card Ten Diamonds
                    ]
                ) $
                evaluate
                    [ Card King Diamonds
                    , Card Jack Diamonds
                    , Card Ace Diamonds
                    , Card Queen Diamonds
                    , Card Ten Diamonds
                    ]



