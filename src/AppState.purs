module AppState
    ( HandList
    , AppStateM
    , run
    , start
    ) where

import Prelude
    ( class Functor, class Apply, class Applicative, class Bind, class Monad
    , Unit
    , const, pure, unit, bind
    , ($), (<$>), (>>=), (==), (<*>), (<>)
    )
import Effect (Effect)
import Effect.Ref (Ref, read, modify_)
import Data.Array as A
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Control.Monad.Reader.Trans (ReaderT, runReaderT, ask, lift)

import Hand as H
import Capabilities
    ( ScoredHand, ScoredSelectedHand
    , clear, error, errors
    , class Edit, class GetHands, class Error, class Nav, class ReadError
    )

data HandList
    = NewHand { editing :: H.Hand, scored :: Array ScoredHand }
    | Editing { editing :: H.Hand, scored :: Array ScoredSelectedHand }
    | NotEditing { scored :: Array ScoredHand }

type AppState =
    { hands :: HandList
    , error :: Maybe (Array String)
    }

start :: AppState
start = { hands : NotEditing { scored : [] }, error : Nothing }

newtype AppStateM x = AppStateM (ReaderT (Ref AppState) Effect x)

derive newtype instance functorAppStateM :: Functor AppStateM
derive newtype instance applyAppStateM :: Apply AppStateM
derive newtype instance applicativeAppStateM :: Applicative AppStateM
derive newtype instance bindAppStateM :: Bind AppStateM
derive newtype instance monadAppStateM :: Monad AppStateM

get :: AppStateM AppState
get = AppStateM $ do
    ref <- ask
    lift $ read ref

modify :: (AppState -> AppState) -> AppStateM Unit
modify f = AppStateM $ do
    ref <- ask
    lift $ modify_ f ref

put :: AppState -> AppStateM Unit
put a = modify (const a)

run :: forall x. Ref AppState -> AppStateM x -> Effect x
run r (AppStateM f) = runReaderT f r

instance getHandsAppStateM :: GetHands AppStateM where
    getAll = get >>= \state -> case state.hands of
        NewHand {editing, scored} -> pure $
            ( \{score, hand} -> {score, hand, selected : false} ) <$> scored
        Editing {editing, scored} -> pure scored
        NotEditing {scored} -> pure $
            ( \{score, hand} -> {score, hand, selected : false} ) <$> scored
    isNewHand = get >>= \state -> case state.hands of
        NewHand _ -> pure true
        _ -> pure false
    isEdit = get >>= \state -> case state.hands of
        NewHand _ -> pure true
        Editing _ -> pure true
        _ -> pure false

instance errorAppStateM :: Error AppStateM where
    errors e = modify $ _ { error = Just e }
    clear = modify $ _ { error = Nothing }

instance navAppStateM :: Nav AppStateM where
    newHand = get >>= \state -> case state.hands of
        NewHand {editing, scored} -> put
            { hands : NewHand $ {editing : H.newHand, scored }
            , error : Nothing
            }
        Editing {editing, scored} -> put
            { hands : NewHand $
                { editing : H.newHand
                , scored : (\{selected,hand,score}->{hand,score}) <$> scored
                }
            , error : Nothing
            }
        NotEditing {scored} -> put
            { hands : NewHand {editing : H.newHand, scored}
            , error : Nothing
            }
    editHand i = get >>= \state -> case state.hands of
        NewHand {editing, scored} -> case A.index scored i of
            Just newEdit -> put
                { hands : Editing
                    { editing : newEdit.hand
                    , scored : A.mapWithIndex ( \j {hand, score} ->
                        {hand, score, selected : i == j} ) scored
                    }
                , error : Nothing
                }
            Nothing -> error "Cannot edit hand, index out of range"
        Editing {editing, scored} -> case A.index scored i of
            Just newEdit | newEdit.selected -> pure unit
            Just newEdit -> put
                { hands : Editing
                    { editing : newEdit.hand
                    , scored : A.mapWithIndex ( \j {hand, score, selected} ->
                        {hand, score, selected : i == j} ) scored
                    }
                , error : Nothing
                }
            Nothing -> error "Cannot edit hand, index out of range"
        NotEditing {scored} -> case A.index scored i of
            Just editing -> put
                { hands : Editing
                    { editing : editing.hand
                    , scored : A.mapWithIndex ( \j {hand, score} ->
                        {hand, score, selected : i == j} ) scored
                    }
                , error : Nothing
                }
            Nothing -> error "Cannot edit hand, index out of range"
    delHand i = get >>= \state -> case state.hands of
        NewHand {editing, scored} -> case A.deleteAt i scored of
            Nothing -> error "Cannot delete hand, index out of range"
            Just newS -> put
                { hands : NewHand {editing, scored : newS}, error : Nothing }
        Editing {editing, scored} ->
            case Tuple <$> A.index scored i <*> A.deleteAt i scored of
                Just (Tuple t newS) | t.selected -> put
                    { hands : NotEditing { scored : (\{selected, hand, score} ->
                        {hand, score}) <$> newS }
                    , error : Nothing
                    }
                Just (Tuple _ newS) -> put
                    { hands : Editing { editing, scored : newS }
                    , error : Nothing
                    }
                Nothing -> error "Cannot delete hand, index out of range"
        NotEditing {scored} -> case A.deleteAt i scored of
            Just newS -> put
                { hands : NotEditing {scored : newS}
                , error : Nothing
                }
            Nothing -> error "Cannot delete hand, index out of range"

instance editAppStateM :: Edit AppStateM where
    getEdit = get >>= \state -> case state.hands of
        NewHand {editing, scored} -> pure $ Just editing
        Editing {editing, scored} -> pure $ Just editing
        NotEditing _ -> pure Nothing
    adjust f = get >>= \state -> case state.hands of
        NewHand {editing, scored} -> modify $
            _ { hands = NewHand {editing : f editing, scored } }
        Editing {editing, scored} -> modify $
            _ { hands = Editing {editing : f editing, scored } }
        NotEditing _ -> clear
    submit = get >>= \state -> case state.hands of
        NewHand {editing, scored} -> case H.scoreHand editing of
            Left e -> errors e
            Right score -> put
                { hands : NotEditing
                    { scored : [{hand : editing, score}] <> scored }
                , error : Nothing
                }
        Editing {editing, scored} -> case H.scoreHand editing of
            Left e -> errors e
            Right nscore -> put
                { hands : NotEditing
                    { scored : ( ( \{hand, score, selected} ->
                            if selected then
                                {hand : editing, score : nscore}
                            else
                                {hand, score} ) <$> scored
                        )
                    }
                , error : Nothing
                }
        NotEditing _ -> clear
    revert = get >>= \state -> case state.hands of
        NewHand {editing, scored} -> put
            {hands : NotEditing {scored}, error : Nothing}
        Editing {editing, scored} -> put
            { hands : NotEditing { scored : ( \{selected, hand, score} ->
                {hand, score} ) <$> scored }
            , error : Nothing
            }
        NotEditing _ -> clear

instance readErrorAppStateM :: ReadError AppStateM where
    readError = _.error <$> get
