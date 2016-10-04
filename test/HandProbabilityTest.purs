module HandProbabilityTest where

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
import HandProbability

testSuite =
    suite "HandProbability" do
        test "" do
            Assert.equal
                3
                3
