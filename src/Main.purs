module Main where

import Prelude (Unit, bind)
import Control.Monad.Eff (Eff)
import Halogen as H
import Halogen.Util (runHalogenAff, awaitBody)
--import ViewOuts (ui, initialState)
import ConvertFracPercRatView (ui, initialState) as CC 
import PotOddsView (ui, initialState) as PV
import Control.Monad.Eff.Random (RANDOM)

main :: forall eff. Eff (H.HalogenEffects (random :: RANDOM | eff) ) Unit
main = runHalogenAff do
    body <- awaitBody
    viewOutsinitialState <- H.fromEff PV.initialState
    let viewOutsUI = PV.ui
    H.runUI viewOutsUI viewOutsinitialState body




    


    
