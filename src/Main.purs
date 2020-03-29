module Main where

import Prelude
    ((==), ($), (<$>), (+), (-), (&&), (<<<), (>=>),
    class Ord, identity, Unit, not, bind, unit, pure, flip, otherwise, map)
import Data.Map as M
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Data.Tuple as T
import Data.Array as A
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Hand
    (Hand, Suit(..), Doubled(..), Honours(..), HonoursType(..),
    newHand, renderHand, setDeclarer, setAllPass, toLevel, setLevel, setSuit,
    toTricks, setTricks, defaultTricks, fromTricks, setDoubled, setHonours,
    validHonours)
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

button :: forall a m. Boolean -> String -> a -> H.ComponentHTML a () m
button true name act = HH.button
    [ HE.onClick $ \_ -> Just act
    , HP.class_ $ HH.ClassName "selected"
    ]
    [ HH.text name ]
button false name act = HH.button
    [ HE.onClick $ \_ -> Just act ]
    [ HH.text name ]

select :: forall a b m. Ord a => (a -> Maybe b) -> (a -> Boolean) -> a ->
    Array (Tuple String a) -> H.ComponentHTML b () m
select act hide sel options = HH.select
    [ HE.onValueChange $ flip M.lookup forwardMap >=> act
    , HP.value $ maybe "- Select one -" identity $ M.lookup sel backwardMap
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

hideHons :: Hand -> Maybe Honours -> Boolean
hideHons _ Nothing = true
hideHons _ (Just None) = false
hideHons hand (Just (Hons _ hons))
    | hand.allPass = true
    | otherwise = case hand.suit of
        Nothing -> false
        Just suit -> not $ validHonours hons suit

renderButtons :: forall m. Hand -> H.ComponentHTML ButtonAction () m
renderButtons hand = HH.div
    [ HP.id_ "buttons" ]
    [ HH.div
        [ HP.class_ $ HH.ClassName "five_buttons" ]
        [ button (not hand.allPass && hand.declarer == Just 'N') "N" $
            SetDeclarer 'N' We
        , button (not hand.allPass && hand.declarer == Just 'E') "E" $
            SetDeclarer 'E' They
        , button (not hand.allPass && hand.declarer == Just 'S') "S" $
            SetDeclarer 'S' We
        , button (not hand.allPass && hand.declarer == Just 'W') "W" $
            SetDeclarer 'W' They
        , button (hand.allPass) "AP" SetAllPass
        ]
    , HH.div
        [ HP.class_ $ HH.ClassName "seven_buttons" ]
        [ button (not hand.allPass && hand.level == toLevel 1) "1" $ SetLevel 1
        , button (not hand.allPass && hand.level == toLevel 2) "2" $ SetLevel 2
        , button (not hand.allPass && hand.level == toLevel 3) "3" $ SetLevel 3
        , button (not hand.allPass && hand.level == toLevel 4) "4" $ SetLevel 4
        , button (not hand.allPass && hand.level == toLevel 5) "5" $ SetLevel 5
        , button (not hand.allPass && hand.level == toLevel 6) "6" $ SetLevel 6
        , button (not hand.allPass && hand.level == toLevel 7) "7" $ SetLevel 7
        ]
    , HH.div
        [ HP.class_ $ HH.ClassName "five_buttons" ]
        [ button (not hand.allPass && hand.suit == Just Clubs) "♣" $
            SetSuit Clubs
        , button (not hand.allPass && hand.suit == Just Diamonds) "♦" $
            SetSuit Diamonds
        , button (not hand.allPass && hand.suit == Just Hearts) "♥" $
            SetSuit Hearts
        , button (not hand.allPass && hand.suit == Just Spades) "♠" $
            SetSuit Spades
        , button (not hand.allPass && hand.suit == Just NoTrumps) "🚫" $
            SetSuit NoTrumps
        ]
    , HH.div
        [ HP.class_ $ HH.ClassName "render_buttons" ]
        [ button false "-" DecreaseTricks
        , HH.text $ renderHand hand
        , button false "+" IncreaseTricks
        ]
    , HH.div
        [ HP.class_ $ HH.ClassName "two_buttons" ]
        [ select (Just <<< SetDouble) (\_ -> false) hand.doubled
            [ Tuple "Undoubled" Undoubled
            , Tuple "Doubled" Doubled
            , Tuple "Redobuled" Redoubled
            ]
        , select (map SetHonours) (hideHons hand) hand.honours
            [ Tuple "No honors" $ Just None
            , Tuple "We have 4" $ Just $ Hons We Four
            , Tuple "We have 5" $ Just $ Hons We Five
            , Tuple "We have aces" $ Just $ Hons We Aces
            , Tuple "They have 4" $ Just $ Hons They Four
            , Tuple "They have 5" $ Just $ Hons They Five
            , Tuple "They have aces" $ Just $ Hons They Aces
            ]
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
