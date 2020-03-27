module Main where

import Prelude (($), not, Unit, bind, unit)

import Data.Maybe (Maybe(..))
import Effect (Effect)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

type State = Boolean

data Action = Toggle

component :: forall q i o m. H.Component HH.HTML q i o m
component = H.mkComponent {
    initialState,
    render,
    eval : H.mkEval $ H.defaultEval { handleAction = handleAction }
}

initialState :: forall i. i -> State
initialState _ = false

render :: forall m. State -> H.ComponentHTML Action () m
render state = HH.button
    [ HP.title label
    , HE.onClick $ \_ -> Just Toggle
    ] [
    HH.text label
    ] where
        label = if state then "Button is pressed" else "Button is not pressed"

handleAction :: forall o m. Action -> H.HalogenM State Action () o m Unit
handleAction Toggle = H.modify_ not

main :: Effect Unit
main = HA.runHalogenAff do
    body <- HA.awaitBody
    runUI component unit body
