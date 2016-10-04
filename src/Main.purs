module Main where

import Prelude (Unit, bind)
import Control.Monad.Eff (Eff)
import Halogen as H
import Halogen.Util (runHalogenAff, awaitBody)
import View (ui, initialState)
import Control.Monad.Eff.Random (RANDOM)

main :: forall eff. Eff (H.HalogenEffects (random :: RANDOM | eff) ) Unit
main = runHalogenAff do
    body <- awaitBody
    state <- H.fromEff initialState
    H.runUI ui state body
