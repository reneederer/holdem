module ConvertNumberTest where

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
import Types (Card(..), Color(..), Face(..), HandValue(..), ConvertNumber(..))
import ConvertNumber

testSuite =
    suite "ConvertNumber" do
        test "Convert Fraction 1" do
            Assert.equal
                [ Fraction 1 4
                , Ratio 3 1
                , Percentage 25.0
                ]
                (convert $ Fraction 1 4)
        test "Convert Fraction 2" do
            Assert.equal
                [ Fraction 3 5
                , Ratio 2 3
                , Percentage 60.0
                ]
                (convert $ Fraction 3 5)
        test "Convert Fraction 3" do
            Assert.equal
                [ Fraction 2 7
                , Ratio 5 2
                , Percentage 28.6
                ]
                (convert $ Fraction 2 7)
        test "Convert Ratio 1" do
            Assert.equal
                [ Ratio 3 1
                , Fraction 1 4
                , Percentage 25.0
                ]
                (convert $ Ratio 3 1)
        test "Convert Ratio 2" do
            Assert.equal
                [ Ratio 2 3
                , Fraction 3 5
                , Percentage 60.0
                ]
                (convert $ Ratio 2 3)
        test "Convert Ratio 3" do
            Assert.equal
                [ Ratio 5 2
                , Fraction 2 7
                , Percentage 28.6
                ]
                (convert $ Ratio 5 2)
        test "Convert Percentage 1" do
            Assert.equal
                [ Percentage 25.0
                , Fraction 1 4
                , Ratio 3 1
                ] $
                convert $ Percentage 25.0
        test "Convert Percentage 2" do
            Assert.equal
                [ Percentage 60.0
                , Fraction 3 5
                , Ratio 2 3
                ] $
                convert $ Percentage 60.0
        test "Convert Percentage 3" do
            Assert.equal
                [ Percentage 28.6
                , Fraction 143 500
                , Ratio 357 143
                ]
                (convert $ Percentage 28.6)
