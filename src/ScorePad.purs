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
    ((==), ($), (<$>), (<$), (<>), (>>>), (&&), not,
    Unit, pure, const, unit, map, identity, show)
import Data.Array as A
import Data.Array.NonEmpty (toArray)
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Hand (HandScore)
import Score (scoreAll, ScoreEntry, ScoreTotal, Team(..), startScore)

type Slot = H.Slot Query Message

data Query a
    = Score (Array HandScore) a

data Action
    = NoAction

data Message

type State = {entries :: Array (Maybe (ScoreEntry String)), totals :: ScoreTotal}

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
initialState = { entries : [], totals : startScore }

splitOnNothings :: forall a. Array (Maybe a) -> Array (Array a)
splitOnNothings =
    A.groupBy sameMaybe >>>
    map (
        toArray >>>
        sequence) >>>
    A.mapMaybe identity where
        sameMaybe Nothing Nothing = true
        sameMaybe (Just _) (Just _) = true
        sameMaybe _ _ = false

renderEntry :: forall m. ScoreEntry String -> H.ComponentHTML Action () m
renderEntry entry = HH.div [] [ HH.text $ show $ entry.amount ]

renderBelow :: forall m. Array (ScoreEntry String) ->
    H.ComponentHTML Action () m
renderBelow entries = HH.div
    [ HP.class_ $ H.ClassName "Below" ]
    [ HH.div [] $ renderEntry <$>
        A.filter (_.below && (\e -> e.team == We)) entries
    , HH.div [] $ renderEntry <$>
        A.filter (_.below && (\e -> e.team == They)) entries
    ]

render :: forall m. State -> H.ComponentHTML Action () m
render state = HH.div
    [ HP.id_ "ScorePad" ]
    $ [ HH.div
        [ HP.id_ "Teams" ]
        [ HH.div [] [ HH.text "We" ]
        , HH.div [] [ HH.text "They" ]
        ]
    , HH.div
        [ HP.id_ "Above" ]
        [ HH.div [] $ renderEntry <$>
            A.filter (not _.below && (\e -> e.team == We))
                (A.mapMaybe identity state.entries)
        , HH.div [] $ renderEntry <$>
            A.filter (not _.below && (\e -> e.team == They))
                (A.mapMaybe identity state.entries)
        ]
    ] <> (renderBelow <$> splitOnNothings state.entries)

handleAction :: forall m. Action -> H.HalogenM State Action () Message m Unit
handleAction NoAction = pure unit

handleQuery :: forall m a. Query a ->
    H.HalogenM State Action () Message m (Maybe a)
handleQuery (Score scores a) = Just a <$ H.put (scoreAll "Rubber bonus" scores)
