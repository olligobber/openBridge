module Buttons
    ( Action(..)
    , Message(..)
    , State
    , Slot
    , Query(..)
    , component
    , initialState
    ) where

import Prelude
    ( (==), ($), (<$>), (+), (-), (&&), (<<<), (>>=), (<$)
    , Unit
    , not, otherwise, map, const, unit, pure, show
    )
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Hand
    (Hand, Suit(..), Doubled(..), Honours(..), HonoursType(..), Level,
    renderHandResult, setDeclarer, setAllPass, allLevels, setLevel, setSuit,
    toTricks, setTricks, defaultTricks, fromTricks, setDoubled, setHonours,
    validHonours)
import Score (Team(..))
import HTMLHelp (button, select)
import Capabilities
    (error, submit, revert, getEdit, adjust, class Edit, class Error)

type Slot = H.Slot Query Message

data Query a = Update a

data Action
    = SetDeclarer Char Team
    | SetAllPass
    | SetLevel Level
    | SetSuit Suit
    | IncreaseTricks
    | DecreaseTricks
    | SetDouble Doubled
    | SetHonours Honours
    | Submit
    | Revert
    | NoAction

type Message = Unit

type State = Maybe Hand

component :: forall m.
    Error m =>
    Edit m =>
    H.Component HH.HTML Query Action Message m
component = H.mkComponent {
    initialState : const initialState,
    render : render,
    eval : H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        }
}

initialState :: State
initialState = Nothing

hideHons :: Hand -> Maybe Honours -> Boolean
hideHons _ Nothing = true
hideHons _ (Just None) = false
hideHons hand (Just (Hons _ hons))
    | hand.allPass = true
    | otherwise = case hand.suit of
        Nothing -> false
        Just suit -> not $ validHonours hons suit

render :: forall m. State -> H.ComponentHTML Action () m
render (Just hand) = HH.div
    [ HP.id_ "buttons" ]
    [ HH.div
        [ HP.class_ $ HH.ClassName "five-buttons" ]
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
        [ HP.class_ $ HH.ClassName "seven-buttons" ]
        $ (\(Tuple num level) ->
            button (not hand.allPass && hand.level == Just level)
                (show num) (SetLevel level)
            ) <$> allLevels
    , HH.div
        [ HP.class_ $ HH.ClassName "five-buttons" ]
        [ button (not hand.allPass && hand.suit == Just Clubs) "♣" $
            SetSuit Clubs
        , button (not hand.allPass && hand.suit == Just Diamonds) "♦" $
            SetSuit Diamonds
        , button (not hand.allPass && hand.suit == Just Hearts) "♥" $
            SetSuit Hearts
        , button (not hand.allPass && hand.suit == Just Spades) "♠" $
            SetSuit Spades
        , button (not hand.allPass && hand.suit == Just NoTrumps) "\\" $
            SetSuit NoTrumps
        ]
    , HH.div
        [ HP.class_ $ HH.ClassName "render-buttons" ]
        [ button false "-" DecreaseTricks
        , HH.div
            [ HP.id_ "hand-result" ]
            [ HH.text $ renderHandResult hand ]
        , button false "+" IncreaseTricks
        ]
    , HH.div
        [ HP.class_ $ HH.ClassName "two-buttons" ]
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
    , HH.div
        [ HP.class_ $ HH.ClassName "two-buttons" ]
        [ button false "Submit" Submit
        , button false "Revert" Revert
        ]
    ]
render Nothing = HH.div [ HP.id_ "buttons" ] []

handleAction :: forall m.
    Error m =>
    Edit m =>
    Action -> H.HalogenM State Action () Message m Unit
handleAction (SetDeclarer name team) = adjust $ setDeclarer name team
handleAction SetAllPass = adjust setAllPass
handleAction (SetLevel level) = adjust $ setLevel level
handleAction (SetSuit suit) = adjust $ setSuit suit
handleAction IncreaseTricks = H.get >>= \h -> case h of
    Nothing -> pure unit
    Just hand ->
        let
            oldtricks = maybe (defaultTricks <$> hand.level) Just hand.tricks
        in case fromTricks <$> oldtricks of
            Nothing -> error "Choose a level before changing the number won"
            Just x -> case toTricks (x+1) of
                Nothing -> error "Maximum number of tricks reached"
                Just newtricks -> adjust $ setTricks newtricks
handleAction DecreaseTricks = H.get >>= \h -> case h of
    Nothing -> pure unit
    Just hand ->
        let
            oldtricks = maybe (defaultTricks <$> hand.level) Just hand.tricks
        in case fromTricks <$> oldtricks of
            Nothing -> error "Choose a level before changing the number won"
            Just x -> case toTricks (x-1) of
                Nothing -> error "Minimum number of tricks reached"
                Just newtricks -> adjust $ setTricks newtricks
handleAction (SetDouble doubling) = adjust $ setDoubled doubling
handleAction (SetHonours hons) = adjust $ setHonours hons
handleAction Submit = submit
handleAction Revert = revert
handleAction NoAction = pure unit

handleQuery :: forall m a.
    Edit m =>
    Query a -> H.HalogenM State Action () Message m (Maybe a)
handleQuery (Update a) = Just a <$ (getEdit >>= H.put)
