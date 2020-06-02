module HandList
    ( SelectedHand
    , Action(..)
    , Message(..)
    , State
    , Slot
    , Query(..)
    , component
    , initialState
    ) where

import Prelude
    ( ($), (<$>), (<>)
    , Unit, pure, otherwise, const, unit, bind, discard
    )
import Data.Maybe (Maybe(..))
import Data.Array as A
import HTMLHelp (button)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Hand (Hand, renderHand)
import Capabilities
    ( getAll, isNewHand, editHand, delHand, isEdit
    , class Nav, class GetHands
    )

type Slot = H.Slot Query Message

data Query a = Update a

data Action
    = EditHand Int
    | DeleteHand Int
    | NoAction

type Message = Unit

type SelectedHand = { hand :: Hand, selected :: Boolean }

type State =
    { handList :: Array SelectedHand
    , newHand :: Boolean
    , editing :: Boolean
    }

component :: forall m.
    Nav m =>
    GetHands m =>
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
initialState = { handList : [], newHand : false, editing : false }

handHTML :: forall m. Int -> SelectedHand -> H.ComponentHTML Action () m
handHTML index {hand, selected}
    | selected = HH.div
        [ HP.class_ $ H.ClassName "hand selected" ]
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
render state
    | state.newHand = HH.div
        [ HP.id_ "hands"
        , HP.class_ $ H.ClassName "small"
        ]
        $ [ HH.div [ HP.class_ $ H.ClassName "hand selected" ] [] ]
            <> A.mapWithIndex handHTML state.handList
    | state.editing = HH.div
        [ HP.id_ "hands"
        , HP.class_ $ H.ClassName "small"
        ]
        $ A.mapWithIndex handHTML state.handList
    | otherwise = HH.div
        [ HP.id_ "hands"
        , HP.class_ $ H.ClassName "big"
        ]
        $ A.mapWithIndex handHTML state.handList

handleAction :: forall m.
    Nav m =>
    Action -> H.HalogenM State Action () Message m Unit
handleAction (EditHand i) = editHand i
handleAction (DeleteHand i) = delHand i
handleAction NoAction = pure unit

handleQuery :: forall m a.
    GetHands m =>
    Query a -> H.HalogenM State Action () Message m (Maybe a)
handleQuery (Update a) = do
    hands <- getAll
    newHand <- isNewHand
    editing <- isEdit
    H.put
        { handList : (\{hand,score,selected} -> {hand,selected}) <$> hands
        , newHand
        , editing
        }
    pure $ Just a
