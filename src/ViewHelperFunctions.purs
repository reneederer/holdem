module ViewHelperFunctions where

import Prelude (class Functor, show, map, (<>), ($))
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as PI
import Data.String (toLower)
import Types (Card(..))

handToImgs ::  forall a b c. (Functor a) => Int -> a Card -> a (HH.HTML b c)
handToImgs width hand =
    map (cardToImg width) hand

cardToImg :: forall a b. Int -> Card -> HH.HTML a b
cardToImg width card = 
    HH.img
        [ PI.src $ getImageFile card
        , PI.alt $ getImageFile card
        , PI.width $ PI.Pixels width ]


getImageFile :: Card -> String
getImageFile (Card face color) = 
    let faceStr = (toLower $ show face)
        colorStr = (toLower $ show color)
    in
    "imgs/" <> faceStr <> "_of_" <> colorStr <> ".svg"



