module TestMain where

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
import EvaluateTest as EvaluateTest
import Evaluate
import ConvertNumberTest as ConvertNumberTest
import HandProbabilityTest as HandProbabilityTest


main :: forall e. Eff (console :: CONSOLE, testOutput :: TESTOUTPUT, avar :: AVAR | e) Unit                      
main = runTest do
    EvaluateTest.testSuite
    ConvertNumberTest.testSuite
    HandProbabilityTest.testSuite
