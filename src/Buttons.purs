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
    ((==), ($), (<$>), (+), (-), (&&), (<<<), (<$), (>>=),
    Unit,
    not, otherwise, map, discard, const, unit, pure, show)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Hand
    (Hand, Suit(..), Doubled(..), Honours(..), HonoursType(..), HandScore,
    Level,
    newHand, renderHandResult, setDeclarer, setAllPass, allLevels, setLevel,
    setSuit, toTricks, setTricks, defaultTricks, fromTricks, setDoubled,
    setHonours, validHonours, scoreHand)
import Score (Team(..))
import HTMLHelp (button, select)

type Slot = H.Slot Query Message

data Query a
    = LoadHand Hand a
    | NewHand a
    | Deactivate a

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

data Message
    = SubmitHand
        { hand :: Hand
        , score :: HandScore
        }
    | RevertHand

data State
    = Active { hand :: Hand, error :: Array String }
    | InActive

component :: forall m. H.Component HH.HTML Query Action Message m
component = H.mkComponent {
    initialState : const initialState,
    render : render,
    eval : H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        }
}

initialState :: State
initialState = InActive

hideHons :: Hand -> Maybe Honours -> Boolean
hideHons _ Nothing = true
hideHons _ (Just None) = false
hideHons hand (Just (Hons _ hons))
    | hand.allPass = true
    | otherwise = case hand.suit of
        Nothing -> false
        Just suit -> not $ validHonours hons suit

render :: forall m. State -> H.ComponentHTML Action () m
render (Active state) = HH.div
    [ HP.id_ "buttons" ]
    [ HH.div
        [ HP.class_ $ HH.ClassName "five_buttons" ]
        [ button (not state.hand.allPass && state.hand.declarer == Just 'N')
            "N" $ SetDeclarer 'N' We
        , button (not state.hand.allPass && state.hand.declarer == Just 'E')
            "E" $ SetDeclarer 'E' They
        , button (not state.hand.allPass && state.hand.declarer == Just 'S')
            "S" $ SetDeclarer 'S' We
        , button (not state.hand.allPass && state.hand.declarer == Just 'W')
            "W" $ SetDeclarer 'W' They
        , button (state.hand.allPass) "AP" SetAllPass
        ]
    , HH.div
        [ HP.class_ $ HH.ClassName "seven_buttons" ]
        $ (\(Tuple num level) ->
            button (not state.hand.allPass && state.hand.level == Just level)
                (show num) (SetLevel level)
            ) <$> allLevels
    , HH.div
        [ HP.class_ $ HH.ClassName "five_buttons" ]
        [ button (not state.hand.allPass && state.hand.suit == Just Clubs)
            "♣" $ SetSuit Clubs
        , button (not state.hand.allPass && state.hand.suit == Just Diamonds)
            "♦" $ SetSuit Diamonds
        , button (not state.hand.allPass && state.hand.suit == Just Hearts)
            "♥" $ SetSuit Hearts
        , button (not state.hand.allPass && state.hand.suit == Just Spades)
            "♠" $ SetSuit Spades
        , button (not state.hand.allPass && state.hand.suit == Just NoTrumps)
            "🚫" $ SetSuit NoTrumps
        ]
    , HH.div
        [ HP.class_ $ HH.ClassName "render_buttons" ]
        [ button false "-" DecreaseTricks
        , HH.text $ renderHandResult state.hand
        , button false "+" IncreaseTricks
        ]
    , HH.div
        [ HP.class_ $ HH.ClassName "two_buttons" ]
        [ select (Just <<< SetDouble) (\_ -> false) state.hand.doubled
            [ Tuple "Undoubled" Undoubled
            , Tuple "Doubled" Doubled
            , Tuple "Redobuled" Redoubled
            ]
        , select (map SetHonours) (hideHons state.hand) state.hand.honours
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
        [ HP.class_ $ HH.ClassName "two_button" ]
        [ button false "Submit" Submit
        , button false "Revert" Revert
        ]
    , HH.div
        [ HP.class_ $ HH.ClassName "button_error" ]
        $ HH.text <$> state.error
    ]
render InActive = HH.div [] []

modifyActive :: ({ hand :: Hand, error :: Array String } ->
    { hand :: Hand, error :: Array String }) -> State -> State
modifyActive f (Active s) = Active $ f s
modifyActive _ InActive = InActive

handleAction :: forall m. Action -> H.HalogenM State Action () Message m Unit
handleAction (SetDeclarer name team) = H.modify_ <<< modifyActive $
    \state -> { hand : setDeclarer name team $ state.hand, error : [] }
handleAction SetAllPass = H.modify_ <<< modifyActive $
    \state -> { hand : setAllPass state.hand, error : [] }
handleAction (SetLevel level) = H.modify_ <<< modifyActive $
    \state -> { hand : setLevel level state.hand, error : [] }
handleAction (SetSuit suit) = H.modify_ <<< modifyActive $
    \state -> { hand : setSuit suit state.hand, error : [] }
handleAction IncreaseTricks = H.get >>= \s -> case s of
    InActive -> pure unit
    Active state -> let
        oldtricks =
            maybe (defaultTricks <$> state.hand.level) Just state.hand.tricks
        in case fromTricks <$> oldtricks of
            Nothing -> H.put $ Active $ state
                { error = ["Choose a level before changing the number won"] }
            Just x -> case toTricks (x+1) of
                Nothing -> H.put $ Active $
                    state { error = ["Maximum number of tricks reached"] }
                Just newtricks -> H.put $ Active $
                    { hand : setTricks newtricks state.hand, error : [] }
handleAction DecreaseTricks = H.get >>= \s -> case s of
    InActive -> pure unit
    Active state -> let
        oldtricks =
            maybe (defaultTricks <$> state.hand.level) Just state.hand.tricks
        in case fromTricks <$> oldtricks of
            Nothing -> H.put $ Active $ state
                { error = ["Choose a level before changing the number won"] }
            Just x -> case toTricks (x-1) of
                Nothing -> H.put $ Active $
                    state { error = ["Minimum number of tricks reached"] }
                Just newtricks -> H.put $ Active
                    { hand : setTricks newtricks state.hand, error : [] }
handleAction (SetDouble doubling) = H.modify_ <<< modifyActive $
    \state -> { hand : setDoubled doubling state.hand, error : [] }
handleAction (SetHonours hons) = H.modify_ <<< modifyActive $
    \state -> { hand : setHonours hons state.hand, error : [] }
handleAction Submit = H.get >>= \s -> case s of
    InActive -> pure unit
    Active state -> case scoreHand state.hand of
        Left error -> H.put $ Active $ state { error = error }
        Right score -> do
            H.raise $ SubmitHand { hand : state.hand, score }
            H.put InActive
handleAction Revert = do
    H.raise RevertHand
    H.put InActive
handleAction NoAction = pure unit

handleQuery :: forall m a. Query a ->
    H.HalogenM State Action () Message m (Maybe a)
handleQuery (LoadHand hand a) = Just a <$ H.put (Active { hand, error : [] })
handleQuery (NewHand a) = Just a <$ H.put
    (Active { hand : newHand, error : [] })
handleQuery (Deactivate a) = Just a <$ H.put InActive
