{-# Language DerivingStrategies #-}
{-# Language FlexibleContexts #-}
{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Reflex.Dom.Popup(
  PopupConfig(..),
  popup,
)
where

import Control.Monad.Fix(MonadFix)
import Control.Monad.IO.Class()
import Reflex.Dom.Core
    ( DomBuilder,
      MonadHold,
      PostBuild(..),
      PerformEvent,
      performEvent_,
      TriggerEvent,
      Reflex(Dynamic),
      Element(..),
      EventName(..),
      Key(Escape),
      keydown,
      wrapDomEvent,
      onEventName,
      DomBuilderSpace,
      RawElement,
      Dynamic,
      mapMaybe,
      Event, GhcjsDomSpace
    )
import Reflex.Dom.Attrs

import Reflex.Dom (ffor2)
import qualified Data.Text as T
import Data.Default (Default(def))
import GHCJS.DOM.EventM (mouseOffsetXY)
import qualified GHCJS.DOM.Types as DOM
import qualified GHCJS.DOM.DOMRectReadOnly as DOM
import qualified GHCJS.DOM.Element as DOM
import JSDOM.Types
    ( MonadJSM
    , HTMLElement(..)
    , uncheckedCastTo
    )
import qualified JSDOM.Types as JSDOM
import JSDOM.Generated.HTMLElement (IsHTMLElement)

data PopupConfig t m = PopupConfig
  { _popupConfig_toggleVisibility :: Dynamic t Bool
  -- ^ Immediately show or hide the popup. Popup can also be dismissed via e.g. clicking outside with role="dialog"
  , _popupConfig_hiddenOrNone :: Bool
  -- ^ True = use visibility:hidden when not visible, False = use display:none when not visible
  , _popupConfig_interiorAttrs :: [Attrs t m]
  -- ^ Convenience field that adds extra attributes to the interior content div, which has class popup-interior
  , _popupConfig_containerAttrs :: [Attrs t m]
  -- ^ Convenience field that adds extra attributes to the popup container div, which has class popup-exterior
  , _popupConfig_zIndex :: Dynamic t (Maybe Int)
  -- ^ z-index CSS property for the popup interior div. If Nothing, sets it to 1000
  }

instance Reflex t => Default (PopupConfig t m) where
  def = PopupConfig
    { _popupConfig_toggleVisibility = pure False
    , _popupConfig_hiddenOrNone = False
    , _popupConfig_interiorAttrs = mempty
    , _popupConfig_containerAttrs = mempty
    , _popupConfig_zIndex = pure Nothing
    }

popup
  :: forall t m a . ( DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadJSM m
     , RawElement (DomBuilderSpace m) ~ JSDOM.Element
     , DomBuilderSpace m ~ GhcjsDomSpace
     , JSDOM.IsHTMLElement JSDOM.Element
     , IsHTMLElement HTMLElement

     )
  => PopupConfig t m
  -> m a
  -> m a

popup cfg widget = do
  let
      -- Note that it's possible for the popup to be dismissed via e.g. clicking outside with role="dialog"
      attrsExterior :: [Attrs t m]
      attrsExterior =
        ("style" ~: ["position" ~:: "relative"]) :  _popupConfig_containerAttrs cfg

      attrsInterior :: [Attrs t m]
      attrsInterior =
        [
         "role" ~: "dialog"
        , "style" ~:
        ffor2 (_popupConfig_toggleVisibility cfg) (_popupConfig_zIndex cfg) (\isVisible zIndex->
            [ "position" ~:: "absolute"
            , "top" ~:: "0"
            , "left" ~:: "0"
            , "z-index" ~:: maybe "1000" T.show zIndex
            , case (isVisible, _popupConfig_hiddenOrNone cfg) of
              (True, _) -> mconcat ["visibility" ~:: "visible", "display" ~:: "block"]
              (False, True) -> "visibility" ~:: "hidden"
              (False, False) -> "display" ~:: "none"
            ]
          )
        ] ++ _popupConfig_interiorAttrs cfg
  elAttrs "div" attrsExterior $ do
    (el,a) <- elAttrs' "div" attrsInterior $ do
      widget
    let pressedEsc :: Event t ()
        pressedEsc = keydown Escape el
        rawEl :: JSDOM.Element
        rawEl = _element_raw el
        htmlElement :: HTMLElement
        htmlElement = JSDOM.toHTMLElement rawEl
    -- relativeCoordsE <- wrapDomEvent htmlElement (onEventName Click) mouseOffsetXY
    -- let areCoordsOutOfBounds :: Event t ()
    --     areCoordsOutOfBounds =
    --       flip mapMaybe relativeCoordsE $ \(x, y) ->
    --         if x < 0 || y < 0 then Just () else Nothing
    let getCoords e = DOM.liftJSM $ do
          rect <- DOM.getBoundingClientRect (_element_raw e)
          y <- DOM.getY rect
          h <- DOM.getHeight rect
          return (y,h)
    pure a
