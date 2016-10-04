module ConvertNumber where

import Prelude (flip, type (~>), class Show, pure, bind, show, map, (<>), ($), (+), mod, (==), (||), (-), (/), (*), (<=), (/=), negate, max, min, (#))
import Data.Array (concatMap, uncons, mapWithIndex, concat, (\\), (..), (:))
import Data.Generic (class Generic, gEq, gShow)
import Data.String (toLower, length, takeWhile, drop, indexOf)
import Math (pow, round)
import Data.Int (floor)
import Data.Int (toNumber)
import Control.Alt ((<|>))
import Control.Apply ((*>))
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Data.Functor (class Functor)
import Control.Monad.Eff.Random (RANDOM, randomInt, random, randomRange)
import Data.Maybe (Maybe(Nothing, Just), fromMaybe)
import Halogen as H
import Halogen.HTML.Events.Indexed as HE
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as PI
import Types (Hand, Card(..), Query(..), ConvertNumber(..), ConvertNumberState)
import Evaluate (bestHand, getOuts, getAllOuts)
import Deal (fullDeck, dealHands)
import Data.Function

render :: ConvertNumberState -> H.ComponentHTML Query
render convertNumberState =
    HH.div_ $
        [ HH.div_ $
            [ HH.text "ConvertNumber"
            , HH.br_
            , HH.br_
            ]
            <>
            (
            convertNumberState.number
            # convert
            # concatMap
                (\n ->
                    if convertNumberState.showSolution || n == convertNumberState.number
                    then
                        [ HH.text $ show n
                        , HH.br_
                        ]
                    else
                        []
                )
            )
            <>
            [ HH.div_
                [ HH.button
                    [ HE.onClick (HE.input_ CreateRandomPuzzle) ]
                    [HH.text "Next random" ]
                , HH.button
                    [ HE.onClick (HE.input_ CreateConvertNumberPuzzle) ]
                    [HH.text "Next same" ]
                , HH.button
                    [ HE.onClick (HE.input_ ShowSolution) ]
                    [HH.text "Show Solution" ]
                ]
            ]
        ]

convert :: ConvertNumber -> Array ConvertNumber
convert number = 
    case number of
        Fraction a b ->
            let g = gcd a b
                n = a / g
                d = b / g
            in
            [ number 
            , Ratio (d-n) n
            , Percentage $ (round ((toNumber a) / (toNumber b) * 1000.0)) / 10.0
            ]
        Ratio a b ->
            let g = gcd a b
                n = a / g
                d = b / g
            in
            [ number
            , Fraction d (n+d)
            , Percentage $ (round ((toNumber d) / ((toNumber n) + (toNumber d)) * 1000.0)) / 10.0
            ]
        Percentage a ->
            let percentageStr = show a
                dotIndex = fromMaybe (-1) $ indexOf "." percentageStr
                dropCount =  max 0 (dotIndex + 1)
                decimalPlaceCount = toNumber $ length $ drop dropCount percentageStr
                s = floor $ a*(10.0 `pow` decimalPlaceCount)
                t = (floor $ 100.0 * (10.0 `pow` decimalPlaceCount))
                commonDivisor = gcd s t
                numerator = s/commonDivisor
                denominator = t/commonDivisor
            in
            [ number
            , Fraction numerator denominator
            , Ratio (denominator-numerator) numerator
            ]


createPuzzle :: forall e. Eff (random::RANDOM | e) ConvertNumberState
createPuzzle = do
    convertNumberType <- randomInt 1 3
    case convertNumberType of
        1 -> do
            r1 <- randomInt 1 10
            r2 <- randomInt 1 9
            let v = { numerator:min r1 r2, denominator:max r1 r2 }
            pure $ { number: Fraction v.numerator v.denominator, showSolution: false }
        2 -> do
            r1 <- randomInt 1 10
            r2 <- randomInt 1 10
            pure $ { number: Ratio r1 r2, showSolution: false }
        _ -> do
            r1 <- randomInt 0 100
            pure $ { number: Percentage $ (toNumber r1)/1.0, showSolution: false }

gcd :: Int -> Int -> Int
gcd a b =
  if a == 0 || b == 0
  then a + b
  else gcd b (a `mod` b)





