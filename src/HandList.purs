module HandList
    ( Action(..)
    , Message(..)
    , State
    , Selection
    , Slot
    , Query(..)
    , component
    , initialState
    ) where

import Prelude
    ((==), ($), (<$>), (-), (<$), (<<<), (<>), (>>=), (>),
    Unit, pure, otherwise, discard, const, unit)
import Data.Array as A
import Data.List (List(..), (:))
import Data.List as L
import Data.Maybe (Maybe(..))
import Effect.Unsafe as Unsafe
import HTMLHelp (button, alert)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Hand (Hand, renderHand, HandScore)

type Slot = H.Slot Query Message

data Query a
    = Set { hand :: Hand, score :: HandScore } a
    | MakeNew a
    | Deselect a

data Action
    = EditHand Int
    | DeleteHand Int
    | NoAction

data Message
    = Edit Hand
    | Score (List HandScore)
    | Deselecting

data Selection
    = Editing Int
    | MakingNew
    | None

type State =
    { hands :: List { hand :: Hand, score :: HandScore }
    , selected :: Selection
    }

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
initialState = { hands : Nil, selected : None }

handHTML :: forall m. Maybe Int -> Int -> Hand -> H.ComponentHTML Action () m
handHTML sel index hand
    | sel == Just index = HH.div
        [ HP.class_ $ H.ClassName "hand"
        , HP.class_ $ H.ClassName "selected"
        ]
        [ HH.text $ renderHand hand
        , button false "Edit" (EditHand index)
        , button false "Delete" (DeleteHand index)
        ]
    | otherwise = HH.div
        [ HP.class_ $ H.ClassName "hand" ]
        [ HH.text $ renderHand hand
        , button false "Edit" (EditHand index)
        , button false "Delete" (DeleteHand index)
        ]

render :: forall m. State -> H.ComponentHTML Action () m
render state = case state.selected of
    MakingNew -> HH.div
        [ HP.id_ "hands" ]
        $ [ HH.div (HP.class_ <<< H.ClassName <$> ["hand", "selected"]) [] ]
            <> A.mapWithIndex (handHTML Nothing)
                (_.hand <$> L.toUnfoldable state.hands)
    None -> HH.div
        [ HP.id_ "hands" ]
        $ A.mapWithIndex (handHTML Nothing)
            (_.hand <$> L.toUnfoldable state.hands)
    Editing i -> HH.div
        [ HP.id_ "hands" ]
        $ A.mapWithIndex (handHTML $ Just i)
            (_.hand <$> L.toUnfoldable state.hands)

handleAction :: forall m. Action -> H.HalogenM State Action () Message m Unit
handleAction (EditHand i) = H.get >>= \state -> case L.index state.hands i of
    Nothing -> pure $ Unsafe.unsafePerformEffect $
        alert "Invalid edit hand button press"
    Just handi -> do
        H.put $ state { selected = Editing i }
        H.raise $ Edit handi.hand
handleAction (DeleteHand i) =
    H.get >>= \state -> case L.deleteAt i state.hands of
        Nothing -> pure $ Unsafe.unsafePerformEffect $
            alert "Invalid delete hand button press"
        Just newHands -> case state.selected of
            Editing j | j > i -> do
                H.put { hands : newHands, selected : Editing $ j-1 }
                H.raise $ Score $ _.score <$> newHands
            Editing j | j == i -> do
                H.put { hands : newHands, selected : None }
                H.raise Deselecting
                H.raise $ Score $ _.score <$> newHands
            _ -> do
                H.modify_ $ _ { hands = newHands }
                H.raise $ Score $ _.score <$> newHands
handleAction NoAction = pure unit

handleQuery :: forall m a. Query a ->
    H.HalogenM State Action () Message m (Maybe a)
handleQuery (Set newHand a) = H.get >>= \state -> case state.selected of
    None -> do
        let _ = Unsafe.unsafePerformEffect $ alert "Invalid query to set hand"
        pure $ Just a
    MakingNew -> do
        H.modify_ $ _ { hands = newHand : state.hands}
        H.raise $ Score $ _.score <$> (newHand : state.hands)
        pure $ Just a
    Editing i -> case L.updateAt i newHand state.hands of
        Nothing -> do
            let _ = Unsafe.unsafePerformEffect $ alert "Invalid hand selection"
            pure $ Just a
        Just newHands -> do
            H.modify_ $ _ { hands = newHands }
            H.raise $ Score $ _.score <$> newHands
            pure $ Just a
handleQuery (Deselect a) = Just a <$ H.modify_ (_ { selected = None } )
handleQuery (MakeNew a) = Just a <$ H.modify_ (_ { selected = MakingNew } )
