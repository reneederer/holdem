module Main where

import Prelude (Unit, bind)
import Control.Monad.Eff (Eff)
import Halogen as H
import Halogen.Util (runHalogenAff, awaitBody)
import ViewOuts (ui, initialState)
import Control.Monad.Eff.Random (RANDOM)

main :: forall eff. Eff (H.HalogenEffects (random :: RANDOM | eff) ) Unit
main = runHalogenAff do
    body <- awaitBody
    viewOutsinitialState <- H.fromEff initialState
    let viewOutsUI = ui
    H.runUI viewOutsUI viewOutsinitialState body




    


    
