module EvaluateTest where

import Prelude (bind, ($), Unit)
import Data.Array ((\\))
import Control.Monad.Eff (Eff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Test.Unit (test, suite)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert
import Deal (fullDeck)
import Types (Card(..), Color(..), Face(..), HandValue(..))
import Evaluate


testSuite =
    suite "Find all Outs" do
        test "Find outs to a higher straight flush" do
            let communityCards = 
                    [ Card Ten Clubs
                    , Card Eight Clubs
                    , Card Seven Clubs
                    ]
            let heroCards = 
                    [ Card Jack Clubs
                    , Card Three Clubs
                    ]
            let villainCards = 
                    [ Card King Clubs
                    , Card Six Clubs
                    ]
            let deck = fullDeck \\ communityCards \\ heroCards \\ villainCards
            Assert.equal
                [ Card Nine Clubs
                ] $ 
                getOuts deck communityCards heroCards [villainCards]
        test "Find outs to a straight flush" do
            let communityCards = 
                    [ Card Two Clubs
                    , Card Four Clubs
                    , Card Two Hearts
                    , Card Three Diamonds
                    ]
            let heroCards = 
                    [ Card Five Clubs
                    , Card Ace Clubs
                    ]
            let villainCards = 
                    [ Card Two Diamonds
                    , Card Two Spades
                    ]
            let deck = fullDeck \\ communityCards \\ heroCards \\ villainCards
            Assert.equal
                [ Card Three Clubs
                ] $ 
                getOuts deck communityCards heroCards [villainCards]
        test "Find outs to four of a kind" do
            let communityCards = 
                    [ Card Two Clubs
                    , Card Two Hearts
                    , Card Three Clubs
                    , Card Three Diamonds
                    ]
            let heroCards = 
                    [ Card Two Spades
                    , Card Ten Hearts
                    ]
            let villainCards = 
                    [ Card Three Hearts
                    , Card Queen Clubs
                    ]
            let deck = fullDeck \\ communityCards \\ heroCards \\ villainCards
            Assert.equal
                [ Card Two Diamonds
                ] $ 
                getOuts deck communityCards heroCards [villainCards]
        test "Find outs to a full house" do
            let communityCards = 
                    [ Card Two Clubs
                    , Card Ace Clubs
                    , Card Three Clubs
                    ]
            let heroCards = 
                    [ Card Ace Spades
                    , Card Three Hearts
                    ]
            let villainCards = 
                    [ Card Jack Clubs
                    , Card Queen Clubs
                    ]
            let deck = fullDeck \\ communityCards \\ heroCards \\ villainCards
            Assert.equal
                [ Card Ace Diamonds
                , Card Ace Hearts
                , Card Three Spades
                , Card Three Diamonds
                ] $ 
                getOuts deck communityCards heroCards [villainCards]
        test "Find outs to a three of a straight" do
            let communityCards = 
                    [ Card Two Clubs
                    , Card Ace Diamonds
                    , Card Ace Spades
                    ]
            let heroCards = 
                    [ Card Three Spades
                    , Card Five Clubs
                    ]
            let villainCards = 
                    [ Card Ace Hearts
                    , Card Six Hearts
                    ]
            let deck = fullDeck \\ communityCards \\ heroCards \\ villainCards
            Assert.equal
                [ Card Four Spades
                , Card Four Clubs
                , Card Four Diamonds
                , Card Four Hearts
                ] $ 
                getOuts deck communityCards heroCards [villainCards]
        test "Find outs to a three of a kind or two pair" do
            let communityCards = 
                    [ Card Two Clubs
                    , Card Nine Diamonds
                    , Card Six Spades
                    ]
            let heroCards = 
                    [ Card Nine Spades
                    , Card Jack Clubs
                    ]
            let villainCards = 
                    [ Card Two Hearts
                    , Card Six Hearts
                    ]
            let deck = fullDeck \\ communityCards \\ heroCards \\ villainCards
            Assert.equal
                [ Card Nine Clubs
                , Card Nine Hearts
                , Card Jack Spades
                , Card Jack Diamonds
                , Card Jack Hearts
                ] $ 
                getOuts deck communityCards heroCards [villainCards]
        test "Find outs to a three of a kind" do
            let communityCards = 
                    [ Card Two Clubs
                    , Card Nine Diamonds
                    , Card Six Spades
                    ]
            let heroCards = 
                    [ Card Three Spades
                    , Card Six Clubs
                    ]
            let villainCards = 
                    [ Card Two Hearts
                    , Card Nine Spades
                    ]
            let deck = fullDeck \\ communityCards \\ heroCards \\ villainCards
            Assert.equal
                [ Card Six Diamonds
                , Card Six Hearts
                ] $ 
                getOuts deck communityCards heroCards [villainCards]
        test "Find outs to two pair" do
            let communityCards = 
                    [ Card Two Clubs
                    , Card Two Diamonds
                    , Card Nine Spades
                    ]
            let heroCards = 
                    [ Card Three Spades
                    , Card Six Clubs
                    ]
            let villainCards = 
                    [ Card Five Hearts
                    , Card Five Spades
                    ]
            let deck = fullDeck \\ communityCards \\ heroCards \\ villainCards
            Assert.equal
                [ Card Six Spades
                , Card Six Diamonds
                , Card Six Hearts
                ] $ 
                getOuts deck communityCards heroCards [villainCards]
        test "Find outs to a pair" do
            let communityCards = 
                    [ Card Two Clubs
                    , Card King Diamonds
                    , Card Eight Spades
                    ]
            let heroCards = 
                    [ Card Ten Spades
                    , Card Six Clubs
                    ]
            let villainCards = 
                    [ Card Nine Hearts
                    , Card Nine Spades
                    ]
            let deck = fullDeck \\ communityCards \\ heroCards \\ villainCards
            Assert.equal
                [ Card Ten Clubs
                , Card Ten Diamonds
                , Card Ten Hearts
                ] $ 
                getOuts deck communityCards heroCards [villainCards]
    --suite "Find best hand out of 7 cards" do
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
    --suite "Recognize 5 cards" do
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


