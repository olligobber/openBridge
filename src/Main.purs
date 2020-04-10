module Main where

import Prelude (Unit, bind, const, pure, unit, ($), (<<<), (<$))
import Effect (Effect)
import Data.Symbol (SProxy(..))
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import HTMLHelp (button)
import Buttons as Buttons
import HandList as HandList
import ScorePad as ScorePad

data Action
    = ButtonsMsg Buttons.Message
    | HandListMsg HandList.Message
    | NewHand
    | None

type State = Unit

type Slots =
    ( buttons :: Buttons.Slot Unit
    , handList :: HandList.Slot Unit
    , scorePad :: ScorePad.Slot Unit
    )

_buttons :: SProxy "buttons"
_buttons = SProxy

_handList :: SProxy "handList"
_handList = SProxy

_scorePad :: SProxy "scorePad"
_scorePad = SProxy

component :: forall q m. H.Component HH.HTML q Action Unit m
component = H.mkComponent
    { initialState : const initialState
    , render
    , eval : H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: State
initialState = unit

render :: forall m. State -> H.ComponentHTML Action Slots m
render _ = HH.div_
    [ HH.slot _scorePad unit ScorePad.component ScorePad.NoAction
        (const $ Just None)
    , button false "Add Hand" NewHand
    , HH.slot _handList unit HandList.component HandList.NoAction
        (Just <<< HandListMsg)
    , HH.slot _buttons unit Buttons.component Buttons.NoAction
        (Just <<< ButtonsMsg)
    ]

handleAction :: forall m. Action -> H.HalogenM State Action Slots Unit m Unit
handleAction (ButtonsMsg (Buttons.SubmitHand hand)) = do
    _ <- H.query _handList unit $ HandList.Set hand unit
    unit <$ (H.query _handList unit $ HandList.Deselect unit)
handleAction (ButtonsMsg Buttons.RevertHand) =
    unit <$ (H.query _handList unit $ HandList.Deselect unit)
handleAction (HandListMsg (HandList.Edit h)) = do
    unit <$ (H.query _buttons unit $ Buttons.LoadHand h unit)
handleAction (HandListMsg (HandList.Score s)) = do
    unit <$ (H.query _scorePad unit $ ScorePad.Score s unit)
handleAction (HandListMsg HandList.Deselecting) =
    unit <$ (H.query _buttons unit $ Buttons.Deactivate unit)
handleAction NewHand = do
    _ <- H.query _handList unit $ HandList.MakeNew unit
    unit <$ (H.query _buttons unit $ Buttons.NewHand unit)
handleAction None = pure unit

main :: Effect Unit
main = HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component None body
