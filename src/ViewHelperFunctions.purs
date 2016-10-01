module ViewHelperFunctions where

import Prelude (class Functor, show, map, (<>), ($))
import Halogen.HTML.Indexed as HH
import Halogen.HTML.Properties.Indexed as PI
import Data.String (toLower)
import Types (Card(..))

handToImgs ::  forall a b c. (Functor a) => a Card -> a (HH.HTML b c)
handToImgs hand =
    map cardToImg hand

cardToImg :: forall a b. Card -> HH.HTML a b
cardToImg card = 
    HH.img
        [ PI.src $ getImageFile card
        , PI.alt $ getImageFile card
        , PI.width $ PI.Pixels 80 ]


getImageFile :: Card -> String
getImageFile (Card face color) = 
    let faceStr = (toLower $ show face)
        colorStr = (toLower $ show color)
    in
    "imgs/" <> faceStr <> "_of_" <> colorStr <> ".svg"
