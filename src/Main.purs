module Main where

import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Prelude (($), (<$>), (+), (-), not, Unit, bind, unit, identity, pure)

import Hand
    (Hand, Suit(..), Doubled(..), Honours(..), HonoursType(..),
    newHand, renderHand, setDeclarer, setAllPass, toLevel, setLevel, setSuit,
    toTricks, setTricks, defaultTricks, fromTricks, setDoubled, setHonours)
import Score (Team(..))

data ButtonAction
    = SetDeclarer Char Team
    | SetAllPass
    | SetLevel Int
    | SetSuit Suit
    | IncreaseTricks
    | DecreaseTricks
    | SetDouble Doubled
    | SetHonours Honours

buttonsComponent :: forall q i o m. H.Component HH.HTML q i o m
buttonsComponent = H.mkComponent {
    initialState : \_ -> newHand,
    render : renderButtons,
    eval : H.mkEval $ H.defaultEval { handleAction = handleButton }
}

button :: forall a m. String -> a -> H.ComponentHTML a () m
button name act = HH.button
    [ HE.onClick $ \_ -> Just act ]
    [ HH.text name ]

option :: forall a m. String -> a -> H.ComponentHTML a () m
option name act = HH.option
    [ HE.onClick $ \_ -> Just act ]
    [ HH.text name ]

renderButtons :: forall m. Hand -> H.ComponentHTML ButtonAction () m
renderButtons hand = HH.div
    [ HP.id_ "buttons" ]
    [ button "N" $ SetDeclarer 'N' We
    , button "E" $ SetDeclarer 'E' They
    , button "S" $ SetDeclarer 'S' We
    , button "W" $ SetDeclarer 'W' They
    , button "AP" SetAllPass
    , button "1" $ SetLevel 1
    , button "2" $ SetLevel 2
    , button "3" $ SetLevel 3
    , button "4" $ SetLevel 4
    , button "5" $ SetLevel 5
    , button "6" $ SetLevel 6
    , button "7" $ SetLevel 7
    , button "♣" $ SetSuit Clubs
    , button "♦" $ SetSuit Diamonds
    , button "♥" $ SetSuit Hearts
    , button "♠" $ SetSuit Spades
    , button "🚫" $ SetSuit NoTrumps
    , button "-" DecreaseTricks
    , HH.text $ renderHand hand
    , button "+" IncreaseTricks
    , HH.select
        [ HP.id_ "double" ]
        [ option "Undoubled" $ SetDouble Undoubled
        , option "Doubled" $ SetDouble Doubled
        , option "Redoubled" $ SetDouble Redoubled
        ]
    , HH.select
        [ HP.id_ "honours" ]
        [ option "No honors" $ SetHonours None
        , option "We have 4" $ SetHonours $ Hons We Four
        , option "We have 5" $ SetHonours $ Hons We Five
        , option "We have aces" $ SetHonours $ Hons We Aces
        , option "They have 4" $ SetHonours $ Hons They Four
        , option "They have 5" $ SetHonours $ Hons They Five
        , option "They have aces" $ SetHonours $ Hons They Aces
        ]
    ]

handleButton :: forall o m. ButtonAction ->
    H.HalogenM Hand ButtonAction () o m Unit
handleButton (SetDeclarer name team) = H.modify_ $ setDeclarer name team
handleButton SetAllPass = H.modify_ setAllPass
handleButton (SetLevel level) = case toLevel level of
    Nothing -> pure unit
    Just lev -> H.modify_ $ setLevel lev
handleButton (SetSuit suit) = H.modify_ $ setSuit suit
handleButton IncreaseTricks = do
    hand <- H.get
    let oldtricks = maybe (defaultTricks <$> hand.level) Just hand.tricks
    case fromTricks <$> oldtricks of
        Nothing -> pure unit
        Just x -> case toTricks (x+1) of
            Nothing -> pure unit
            Just newtricks -> H.modify_ $ setTricks newtricks
handleButton DecreaseTricks = do
    hand <- H.get
    let oldtricks = maybe (defaultTricks <$> hand.level) Just hand.tricks
    case fromTricks <$> oldtricks of
        Nothing -> pure unit
        Just x -> case toTricks (x-1) of
            Nothing -> pure unit
            Just newtricks -> H.modify_ $ setTricks newtricks
handleButton (SetDouble doubling) = H.modify_ $ setDoubled doubling
handleButton (SetHonours hons) = H.modify_ $ setHonours hons

component :: forall q i o m. H.Component HH.HTML q i o m
component = buttonsComponent

render :: forall m. Hand -> H.ComponentHTML ButtonAction () m
render = renderButtons

main :: Effect Unit
main = HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body
