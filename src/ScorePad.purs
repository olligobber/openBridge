module ScorePad
    ( Action(..)
    , Message(..)
    , State
    , Slot
    , Query(..)
    , component
    , initialState
    ) where

import Prelude
    ( (==), ($), (<$>), (<>), (>>>), (&&), not
    , Unit, pure, const, unit, map, identity, show, bind, discard
    )
import Data.Array as A
import Data.Array.NonEmpty (toArray)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Score (scoreAll, ScoreEntry, ScoreTotal, Team(..), startScore)
import Capabilities (getAll, class GetHands)

type Slot = H.Slot Query Message

data Query a = Update a

data Action = NoAction

data Message

type State =
    { entries :: Array (Maybe (ScoreEntry String)), totals :: ScoreTotal }

component :: forall m. GetHands m => H.Component HH.HTML Query Action Message m
component = H.mkComponent {
    initialState : const initialState,
    render : render,
    eval : H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        }
}

initialState :: State
initialState = { entries : [], totals : startScore }

splitOnNothings :: forall a. Array (Maybe a) -> Array (Maybe (Array a))
splitOnNothings =
    A.groupBy sameMaybe >>>
    map (
        toArray >>>
        sequence) where
        sameMaybe Nothing Nothing = true
        sameMaybe (Just _) (Just _) = true
        sameMaybe _ _ = false

renderEntry :: forall m. ScoreEntry String -> H.ComponentHTML Action () m
renderEntry entry = HH.div [] [ HH.text $ show $ entry.amount ]

renderBelow :: forall m. Maybe (Array (ScoreEntry String)) ->
    H.ComponentHTML Action () m
renderBelow Nothing = HH.tr
    [ HP.class_ $ H.ClassName "game" ]
    [ HH.td [ HP.class_ $ H.ClassName "we" ] []
    , HH.td [ HP.class_ $ H.ClassName "they" ] []
    ]
renderBelow (Just entries) = HH.tr
    [ HP.class_ $ H.ClassName "below" ]
    [ HH.td [ HP.class_ $ H.ClassName "we" ] $ renderEntry <$>
        A.filter (_.below && (\e -> e.team == We)) entries
    , HH.td [ HP.class_ $ H.ClassName "they" ] $ renderEntry <$>
        A.filter (_.below && (\e -> e.team == They)) entries
    ]

render :: forall m. State -> H.ComponentHTML Action () m
render state = HH.table
    [ HP.id_ "scorepad" ]
    $ [ HH.tr
        [ HP.id_ "teams" ]
        [ HH.th [ HP.class_ $ H.ClassName "we" ] [ HH.text "We" ]
        , HH.th [ HP.class_ $ H.ClassName "they" ] [ HH.text "They" ]
        ]
    , HH.tr
        [ HP.id_ "above" ]
        [ HH.td [ HP.class_ $ H.ClassName "we" ] $ renderEntry <$>
            A.filter (not _.below && (\e -> e.team == We))
                (A.mapMaybe identity state.entries)
        , HH.td [ HP.class_ $ H.ClassName "they" ] $ renderEntry <$>
            A.filter (not _.below && (\e -> e.team == They))
                (A.mapMaybe identity state.entries)
        ]
    ] <> (renderBelow <$> splitOnNothings state.entries)
    <> [ HH.tr
        [ HP.class_ $ H.ClassName "pad" ]
        [ HH.td [ HP.class_ $ H.ClassName "we" ] []
        , HH.td [ HP.class_ $ H.ClassName "they" ] []
        ]
    , HH.tr
        [ HP.id_ "totals" ]
        [ HH.td
            [ HP.class_ $ H.ClassName "we" ]
            [ HH.text $ show $ state.totals.total.we ]
        , HH.td
            [ HP.class_ $ H.ClassName "they" ]
            [ HH.text $ show $ state.totals.total.they ]
        ]
    ]

handleAction :: forall m. Action -> H.HalogenM State Action () Message m Unit
handleAction NoAction = pure unit

handleQuery :: forall m a.
    GetHands m =>
    Query a -> H.HalogenM State Action () Message m (Maybe a)
handleQuery (Update a) = do
    handList <- getAll
    let scores = _.score <$> handList
    H.put (scoreAll "Rubber bonus" scores)
    pure $ Just a
