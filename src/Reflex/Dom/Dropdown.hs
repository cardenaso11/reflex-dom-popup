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
import Data.Text (Text)
import Reflex.Dom.Core
    ( DomBuilder,
      MonadHold,
      PostBuild(..),
      PerformEvent,
      TriggerEvent,
      Reflex(Dynamic)
    )
import qualified Data.Map as M
import Reflex.Dom.Attrs

import Reflex.Dom (ffor)

data PopupConfig t m = PopupConfig
  { _popupConfig_visible :: Dynamic t Bool
  -- ^ Immediately show or hide the popup
  , _popupConfig_hiddenOrNone :: Bool
  -- ^ True = use visibility:hidden when not visible, False = use display:none when not visible
  , _popupConfig_interior :: [Attrs t m]
  -- ^ Convenience field that adds extra attributes to the interior content div, which has class popup-interior
  , _popupConfig_exterior :: [Attrs t m]
  -- ^ Convenience field that adds extra attributes to the popup container div, which has class popup-exterior
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
        ("style" ~: ["position" ~:: "relative"]) : _popupConfig_exterior cfg

      attrsInterior :: [Attrs t m]
      attrsInterior =
        [
         "role" ~: "dialog"
        , "style" ~:
        ffor (_popupConfig_visible cfg) (\isVisible ->
            [ "position" ~:: "absolute"
            , case (isVisible, _popupConfig_hiddenOrNone cfg) of
              (True, _) -> mconcat ["visibility" ~:: "visible", "display" ~:: "inline-block"]
              (False, True) -> "visibility" ~:: "hidden"
              (False, False) -> "display" ~:: "none"
            ]
          )
        ] ++ _popupConfig_interior cfg
  elAttrs "div" attrsExterior $ do
    elAttrs "div" attrsInterior $ do
      widget