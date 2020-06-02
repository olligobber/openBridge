module Main where

import Prelude
    ( Unit
    , bind, const, pure, unit, discard
    , ($), (<<<), (<$>), (>>=)
    )
import Effect (Effect)
import Data.Symbol (SProxy(..))
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)
import Effect.Ref as R

import HTMLHelp (button)
import Buttons as Buttons
import HandList as HandList
import ScorePad as ScorePad
import Capabilities
    ( class GetHands, class Nav, class Error, class ReadError, class Edit
    , newHand, clear, readError
    )
import AppState (AppStateM, run, start)

data Action
    = ButtonsMsg Unit
    | HandListMsg Unit
    | NewHand
    | ClearError
    | None

type State = Maybe (Array String)

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

component :: forall q. H.Component HH.HTML q Action Unit AppStateM
component = H.mkComponent
    { initialState : const initialState
    , render
    , eval : H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: State
initialState = Nothing

render :: forall m.
    GetHands m =>
    Nav m =>
    Error m =>
    Edit m =>
    State -> H.ComponentHTML Action Slots m
render Nothing = HH.div
    [ HP.id_ "main" ]
    [ HH.slot _scorePad unit ScorePad.component ScorePad.NoAction
        (const $ Just None)
    , HH.div
        [ HP.id_ "rightpanel" ]
        [ HH.div
            [ HP.id_ "menu" ]
            [ button false "Add Hand" NewHand ]
        , HH.slot _handList unit HandList.component HandList.NoAction
            (Just <<< HandListMsg)
        , HH.slot _buttons unit Buttons.component Buttons.NoAction
            (Just <<< ButtonsMsg)
        ]
    ]
render (Just e) = HH.div
    [ HP.id_ "main" ]
    [ HH.slot _scorePad unit ScorePad.component ScorePad.NoAction
        (const $ Just None)
    , HH.div
        [ HP.id_ "rightpanel" ]
        [ HH.div
            [ HP.id_ "menu" ]
            [ button false "Add Hand" NewHand ]
        , HH.slot _handList unit HandList.component HandList.NoAction
            (Just <<< HandListMsg)
        , HH.slot _buttons unit Buttons.component Buttons.NoAction
            (Just <<< ButtonsMsg)
        ]
    , HH.div
        [ HP.id_ "errors"
        , HE.onClick $ \_ -> Just ClearError
        ]
        $ (\err -> HH.div [] [ HH.text err ]) <$> e
    ]

handleAction :: forall m.
    Error m =>
    ReadError m =>
    Nav m =>
    Action -> H.HalogenM State Action Slots Unit m Unit
handleAction (ButtonsMsg _) = do
    _ <- H.query _handList unit $ HandList.Update unit
    _ <- H.query _buttons unit $ Buttons.Update unit
    _ <- H.query _scorePad unit $ ScorePad.Update unit
    readError >>= H.put
handleAction (HandListMsg _) = do
    _ <- H.query _handList unit $ HandList.Update unit
    _ <- H.query _buttons unit $ Buttons.Update unit
    _ <- H.query _scorePad unit $ ScorePad.Update unit
    readError >>= H.put
handleAction NewHand = do
    newHand
    _ <- H.query _handList unit $ HandList.Update unit
    _ <- H.query _buttons unit $ Buttons.Update unit
    _ <- H.query _scorePad unit $ ScorePad.Update unit
    readError >>= H.put
handleAction ClearError = do
    clear
    _ <- H.query _handList unit $ HandList.Update unit
    _ <- H.query _buttons unit $ Buttons.Update unit
    _ <- H.query _scorePad unit $ ScorePad.Update unit
    readError >>= H.put
handleAction None = pure unit

main :: Effect Unit
main = HA.runHalogenAff do
    body <- HA.awaitBody
    ref <- H.liftEffect $ R.new start
    runUI (H.hoist (H.liftEffect <<< run ref) component) None body
