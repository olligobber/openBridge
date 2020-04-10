module HTMLHelp
    ( button
    , select
    , alert
    ) where

import Prelude
    (Unit, class Ord, ($), (<$>), (>=>), (>>=), flip, identity, (<<<), not)
import Effect (Effect)
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Data.Tuple as T
import Data.Array as A
import Web.HTML (window)
import Web.HTML.Window as W
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

-- Make a button given its state, name, and action when pressed
button :: forall r a m. Boolean -> String -> a -> H.ComponentHTML a r m
button true name act = HH.button
    [ HE.onClick $ \_ -> Just act
    , HP.class_ $ HH.ClassName "selected"
    ]
    [ HH.text name ]
button false name act = HH.button
    [ HE.onClick $ \_ -> Just act ]
    [ HH.text name ]

{-
    Make a select dropdown given the action function, the hide function,
    the default option, and a list of options
-}
select :: forall a b r m. Ord a => (a -> Maybe b) -> (a -> Boolean) -> a ->
    Array (Tuple String a) -> H.ComponentHTML b r m
select act hide sel options = HH.select
    [ HE.onValueChange $ flip M.lookup forwardMap >=> act
    , HP.value $ maybe "" identity $ M.lookup sel backwardMap
    ]
    $ option <$> A.filter (not <<< hide <<< T.snd) options
    where
        forwardMap = M.fromFoldable options
        backwardMap = M.fromFoldable $ T.swap <$> options
        option (Tuple name acts) = HH.option
            [ HP.value name
            , HP.disabled $ hide acts
            ]
            [ HH.text name ]

-- Pop up alert
alert :: String -> Effect Unit
alert s = window >>= W.alert s
