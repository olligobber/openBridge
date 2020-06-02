module Capabilities
    ( ScoredHand
    , ScoredSelectedHand
    , class GetHands
    , getAll
    , isNewHand
    , isEdit
    , class Error
    , errors
    , clear
    , error
    , class Nav
    , newHand
    , editHand
    , delHand
    , class Edit
    , getEdit
    , adjust
    , submit
    , revert
    , class ReadError
    , readError
    ) where

import Prelude (class Monad, Unit, (<<<), (<*), pure, unit)
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift, raise)

import Hand (Hand, HandScore)

type ScoredHand = { hand :: Hand, score :: HandScore }

type ScoredSelectedHand =
    { selected :: Boolean
    , hand :: Hand
    , score :: HandScore
    }

class Monad m <= GetHands m where
    getAll :: m (Array ScoredSelectedHand)
    isNewHand :: m Boolean
    isEdit :: m Boolean

instance getHandsHalogenM :: GetHands m => GetHands (HalogenM s a t o m) where
    getAll = lift getAll
    isNewHand = lift isNewHand
    isEdit = lift isEdit

class Monad m <= Error m where
    errors :: Array String -> m Unit
    clear :: m Unit

instance errorHalogenM :: Error m => Error (HalogenM s a t Unit m) where
    errors e = lift (errors e) <* raise unit
    clear = lift clear <* raise unit

error :: forall m. Error m => String -> m Unit
error = errors <<< pure

class Monad m <= Nav m where
    newHand :: m Unit
    editHand :: Int -> m Unit
    delHand :: Int -> m Unit

instance navHalogenM :: Nav m => Nav (HalogenM s a t Unit m) where
    newHand = lift newHand <* raise unit
    editHand i = lift (editHand i) <* raise unit
    delHand i = lift (delHand i) <* raise unit

class Monad m <= Edit m where
    getEdit :: m (Maybe Hand)
    adjust :: (Hand -> Hand) -> m Unit
    submit :: m Unit
    revert :: m Unit

instance editHalogenM :: Edit m => Edit (HalogenM s a t Unit m) where
    getEdit = lift getEdit
    adjust f = lift (adjust f) <* raise unit
    submit = lift submit <* raise unit
    revert = lift revert <* raise unit

class Monad m <= ReadError m where
    readError :: m (Maybe (Array String))

instance readErrorHalogenM :: ReadError m =>
    ReadError (HalogenM s a t o m) where
        readError = lift readError
