module HtmlUtils where

import Prelude

import Core.YearMonth (YearMonth)
import Data.Maybe (Maybe(..), isJust)
import Data.String.Utils as String
import Effect (Effect)
import Halogen (AttrName(..), ClassName(..), ElemName(..), PropName(..))
import Halogen.HTML (HTML, IProp, element)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Utils as Utils
import Web.Event.Event (Event)
import Web.Event.Event as E
import Web.Event.EventTarget (EventTarget)
import Web.HTML as HTML
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.HTMLInputElement as HTMLInputElement
import Web.HTML.HTMLTextAreaElement as HTMLTextAreaElement
import Web.HTML.Location as Location
import Web.HTML.Window as Window
import Web.URL as URL

classes' :: forall r i. String -> IProp (class :: String | r) i
classes' str =
  HP.classes $ ClassName <$> String.words str

addClassIf :: Boolean -> String -> String -> String
addClassIf cond className classes =
  if cond then classes <> " " <> className else classes

addStringWhenJust :: forall @a. Maybe a -> (a -> String) -> String -> String
addStringWhenJust ma mkString str =
  case ma of
    Nothing -> str
    Just a -> str <> mkString a

tooltip :: forall r i. String -> IProp r i
tooltip text = HH.attr (AttrName "data-tooltip") text

-- NOTE: Halogen's `input` only allows setting a `max` property with a number.
-- So I created a custom `input` that doesn't have any restrictions on properties.
--
-- Bug reported here: https://github.com/purescript-halogen/purescript-halogen/issues/837
input' :: forall w r i. Array (IProp r i) -> HTML w i
input' props = element (ElemName "input") props []

maxYearMonth :: forall r i. YearMonth -> IProp (max :: YearMonth | r) i
maxYearMonth = HP.prop (PropName "max")

minYearMonth :: forall r i. YearMonth -> IProp (min :: YearMonth | r) i
minYearMonth = HP.prop (PropName "min")

-- An Iconify icon: https://iconify.design/
--
-- See also:
--    https://iconify.design/docs/usage/
--    https://iconify.design/docs/icon-components/
--    Registering the web component: https://iconify.design/docs/iconify-icon/#registering-the-web-component
iconify :: forall w r i. String -> Array (IProp r i) -> HTML w i
iconify iconId props =
  HH.element
    (HH.ElemName "iconify-icon")
    ([ HH.attr (HH.AttrName "icon") iconId ] <> props)
    []

displayIf :: forall w i. Boolean -> HTML w i -> HTML w i
displayIf cond html =
  if cond then html else HH.text ""

displayWhenJust :: forall a w i. Maybe a -> (a -> HTML w i) -> HTML w i
displayWhenJust m f =
  case m of
    Just x -> f x
    Nothing -> HH.text ""

foreign import _log :: forall a. a -> Effect Unit

jsLog :: forall a. a -> Effect Unit
jsLog cd = do
  _log cd

-- Check whether an event was triggered from an input, textarea, or contenteditable element.
isInputElement :: Event -> Effect Boolean
isInputElement event = do
  let
    eventTarget =
      event
        # E.target
        # Utils.unsafeFromJust "Failed to get focus event's target"

  contentEditable <- isContentEditable eventTarget

  pure $ isInput eventTarget || isTextArea eventTarget || contentEditable

  where
  isInput :: EventTarget -> Boolean
  isInput target = isJust (HTMLInputElement.fromEventTarget target)

  isTextArea :: EventTarget -> Boolean
  isTextArea target = isJust (HTMLTextAreaElement.fromEventTarget target)

  isContentEditable :: EventTarget -> Effect Boolean
  isContentEditable target = do
    case HTMLElement.fromEventTarget target of
      Nothing -> pure false
      Just elem -> HTMLElement.isContentEditable elem

-- Returns the base URL for the backend
apiBaseUrl :: Effect String
apiBaseUrl = do
  loc <- HTML.window >>= Window.location
  href <- Location.href loc
  case URL.fromAbsolute href of
    Nothing -> pure href
    Just url -> do
      -- The window.location.href could be:
      --  * http://localhost:1234/ if we're running in live reload mode with `just run`, in which case the backend is available on `http://localhost:8081/`
      --  * http://localhost:8081/ if the static files are being served by the servant server
      --  * http://192.168.1.244:8082/ if the static files are being served by the systemd service, from the raspberry pi's nix store
      --
      -- In the 1st case, we want to replace the port with 8081, and use that URL to send requests to the backend.
      -- In all other cases, we want to use the same base URL.
      let adjustedUrl = if URL.port url == "1234" then URL.setPort "8081" url else url
      pure $ URL.origin adjustedUrl <> "/"

frontendBaseUrl :: Effect String
frontendBaseUrl = do
  loc <- HTML.window >>= Window.location
  href <- Location.href loc
  case URL.fromAbsolute href of
    Nothing -> pure href
    Just url -> pure $ URL.origin url <> "/"

redirectTo :: String -> Effect Unit
redirectTo url = do
  location <- Window.location =<< HTML.window
  Location.setHref url location
