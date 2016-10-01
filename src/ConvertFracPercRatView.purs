module ConvertFracPercRatView where

import Prelude (flip, type (~>), class Show, pure, bind, show, map, (<>), ($), (+), mod, (==), (||), (-), (/), (*), (<=), (/=), negate, max)
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
import Types (Hand, Card(..))
import Evaluate (bestHand, getOuts, getAllOuts)
import Deal (fullDeck, dealHands)

data ConvertEntity = 
      Fraction Int Int
    | Ratio Int Int
    | Percentage Number
derive instance genericConvertEntity :: Generic ConvertEntity
instance showConvertEntity :: Show ConvertEntity where
    show convertEntity =
        case convertEntity of
            Fraction numerator denominator -> show numerator <> " / " <> show denominator
            Ratio numerator denominator -> show numerator <> " : " <> show denominator
            Percentage percent -> show percent <> " %"


type State =
    { convertEntity:: ConvertEntity }

initialState :: forall e. Eff (random :: RANDOM | e) State
initialState = do
    e <- createRandomConvertEntity
    pure { convertEntity: e }

data Query a =
      NewConvertEntity a

ui :: forall eff. H.Component State Query (Aff (random :: RANDOM | eff))
ui =
    H.component { render, eval }
    where
    render :: State -> H.ComponentHTML Query
    render state =
        HH.div_ $
            [ HH.div_ $
                [ HH.text $ show state.convertEntity
                , HH.br_
                , HH.br_
                ]
                <>
                (concatMap
                    (\e ->
                        [ HH.text $ show e
                        , HH.br_
                        ]
                    ) $ convert state.convertEntity
                )
                <>
                [ HH.div_
                    [ HH.button
                        [ HE.onClick (HE.input_ NewConvertEntity) ]
                        [HH.text "Next" ]
                    ]
                ]
            ]

    eval :: Query ~> H.ComponentDSL State Query (Aff (random :: RANDOM | eff))
    eval (NewConvertEntity next) = do
        convertEntity <- H.fromEff createRandomConvertEntity
        H.modify (\state -> state {convertEntity=convertEntity})
        pure next


convert :: ConvertEntity -> Array ConvertEntity
convert convertEntity = 
    case convertEntity of
        Fraction a b ->
            let g = gcd a b
                n = a / g
                d = b / g
            in
            [ Ratio (d-n) n
            , Percentage $ (round ((toNumber a) / (toNumber b) * 1000.0)) / 10.0
            ]
        Ratio a b ->
            let g = gcd a b
                n = a / g
                d = b / g
            in
            [ Fraction d (n+d)
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
            [ Fraction numerator denominator
            , Ratio (denominator-numerator) numerator
            ]


createRandomConvertEntity :: forall e. Eff (random::RANDOM | e) ConvertEntity
createRandomConvertEntity = do
    convertEntityTypeIndicator <- randomInt 1 3
    case convertEntityTypeIndicator of
        1 -> do
            numerator <- randomInt 1 10
            denominator <- randomInt 1 9
            if numerator <= denominator
                then
                    pure $ Fraction numerator denominator
                else 
                    pure $ Fraction denominator numerator 
        2 -> do
            numerator <- randomInt 1 10
            denominator <- randomInt 1 9
            if numerator <= denominator
                then
                    pure $ Ratio denominator numerator
                else 
                    pure $ Ratio numerator denominator 
        _ -> do
            percentage <- randomInt 10 1000
            pure $ Percentage $ (toNumber percentage) / 10.0

gcd :: Int -> Int -> Int
gcd a b =
  if a == 0 || b == 0
  then a + b
  else gcd b (a `mod` b)





