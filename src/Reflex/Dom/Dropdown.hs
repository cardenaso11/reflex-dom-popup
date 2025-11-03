{-# Language DerivingStrategies #-}
{-# Language FlexibleContexts #-}
{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# LANGUAGE MultilineStrings #-}

module Reflex.Dom.Dropdown (
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
      TriggerEvent,
      Reflex(Dynamic)
    )
import Reflex.Dom.Attrs

import Reflex.Dom (ffor, ffor2)
import qualified Data.Text as T
import Debug.Trace (traceShow)

data PopupConfig t m = PopupConfig
  { _popupConfig_visible :: Dynamic t Bool
  -- ^ Immediately show or hide the popup
  , _popupConfig_hiddenOrNone :: Bool
  -- ^ True = use visibility:hidden when not visible, False = use display:none when not visible
  , _popupConfig_interiorAttrs :: [Attrs t m]
  -- ^ Convenience field that adds extra attributes to the interior content div, which has class popup-interior
  , _popupConfig_containerAttrs :: [Attrs t m]
  -- ^ Convenience field that adds extra attributes to the popup container div, which has class popup-exterior
  -- , _popupConfig_zIndex :: Dynamic t (Maybe Integer)
  -- -- ^ Optional z-index to apply to the popup interior div
  }

popup
  :: forall t m a . ( DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     , PerformEvent t m
     , TriggerEvent t m
     )
  => PopupConfig t m
  -> m a
  -> m a

popup cfg widget = do
  let
      attrsExterior :: [Attrs t m]
      attrsExterior =
        ("style" ~: ["position" ~:: "relative"]) : traceShow ("CONTAINER_ATTRS:", map attrs_style (_popupConfig_containerAttrs cfg)) (_popupConfig_containerAttrs cfg)

      attrsInterior :: [Attrs t m]
      attrsInterior =
        [
         "role" ~: "dialog"
        , "style" ~:
        ffor (_popupConfig_visible cfg) {- (_popupConfig_zIndex cfg) -} (\isVisible {-zIndex-} ->
            [ "position" ~:: "absolute"
            -- , "z-index" ~:: maybe "auto" T.show zIndex
            , case (isVisible, _popupConfig_hiddenOrNone cfg) of
              (True, _) -> mconcat ["visibility" ~:: "visible", "display" ~:: "inline-block"]
              (False, True) -> "visibility" ~:: "hidden"
              (False, False) -> "display" ~:: "none"
            ]
          )
        ] ++ _popupConfig_interiorAttrs cfg
  elAttrs "div" attrsExterior $ do
    elAttrs "div" attrsInterior $ do
      widget