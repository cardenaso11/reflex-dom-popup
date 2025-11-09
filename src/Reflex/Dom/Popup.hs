{-# Language DerivingStrategies #-}
{-# Language FlexibleContexts #-}
{-# Language OverloadedStrings #-}
{-# Language ScopedTypeVariables #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}


module Reflex.Dom.Popup(
  PopupConfig(..),
  popup,
  defaultPopupGap,
  pBelow,
  pBelowWithGap,
  pAbove,
  pAboveWithGap,
  pRight,
  pRightWithGap,
  pLeft,
  pLeftWithGap,
  pBottomLeft,
  pBottomLeftWithGap,
  pBottomRight,
  pBottomRightWithGap,
  pTopLeft,
  pTopLeftWithGap,
  pTopRight,
  pTopRightWithGap,
  pCentered
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
      constDyn,
      mapMaybe,
      Event, ffor
    )
import Reflex.Dom.Attrs

import Reflex.Dom (ffor2)
import qualified Data.Text as T
import Data.Default (Default(def))
import JSDOM.Types (MonadJSM)
import Control.Applicative (liftA3)
import Data.Maybe (fromMaybe)

data PopupConfig t m = PopupConfig
  { _popupConfig_toggleVisibility :: Dynamic t Bool
  -- ^ Immediately show or hide the popup. Popup can also be dismissed via e.g. clicking outside with role="dialog"
  , _popupConfig_hiddenOrNone :: Dynamic t Bool
  -- ^ True = use visibility:hidden when not visible, False = use display:none when not visible
  , _popupConfig_interiorAttrs :: [Attrs t m]
  -- ^ Convenience field that adds extra attributes to the interior content div, which has class popup-interior
  , _popupConfig_containerAttrs :: [Attrs t m]
  -- ^ Convenience field that adds extra attributes to the popup container div, which has class popup-exterior
  , _popupConfig_zIndex :: Dynamic t (Maybe Int)
  -- ^ z-index CSS property for the popup interior div. If Nothing, sets it to 1000
  , _popupConfig_topGapFromContainer :: Dynamic t (Maybe T.Text)
  -- ^ Gap from the top of the popup container to the top of the popup interior. You can use any CSS math expression. Default is Nothing, which means 0 gap.
  , _popupConfig_leftGapFromContainer :: Dynamic t (Maybe T.Text)
  -- ^ Gap between the leftmost of the popup container and the leftmost of the popup interior. You can use any CSS math expression. Default is Nothing, which means 0 gap.
  }

instance Reflex t => Default (PopupConfig t m) where
  def = PopupConfig
    { _popupConfig_toggleVisibility = pure False
    , _popupConfig_hiddenOrNone = pure False
    , _popupConfig_interiorAttrs = mempty
    , _popupConfig_containerAttrs = mempty
    , _popupConfig_zIndex = pure Nothing
    , _popupConfig_topGapFromContainer = pure Nothing
    , _popupConfig_leftGapFromContainer = pure Nothing
    }

popup
  :: forall t m a . ( DomBuilder t m
     , MonadFix m
     , MonadHold t m
     , PostBuild t m
     , PerformEvent t m
     , TriggerEvent t m
     , MonadJSM m
     )
  => PopupConfig t m
  -> m a
  -> m (Event t (), a)
  -- ^ Returns an event that fires when the popup could be considered dismissed (e.g. via pressing Escape).
  -- This event does not automatically hide the popup, but it can be fed back into the popup to hide it.

popup cfg widget = do
  let
      attrsExterior :: [Attrs t m]
      attrsExterior =
        ("style" ~: ["position" ~:: "relative"]) :  _popupConfig_containerAttrs cfg

      configCombinedDyn :: Dynamic t (Bool, Bool, Maybe Int, Maybe T.Text, Maybe T.Text)
      configCombinedDyn = (,,,,)
        <$> _popupConfig_toggleVisibility cfg
        <*> _popupConfig_hiddenOrNone cfg
        <*> _popupConfig_zIndex cfg
        <*> _popupConfig_topGapFromContainer cfg
        <*> _popupConfig_leftGapFromContainer cfg
      attrsInterior :: [Attrs t m]
      attrsInterior =
        ("style" ~:
        ffor configCombinedDyn (\(isVisible,hiddenOrNone,zIndex, topGap, leftGap)->
            [ "position" ~:: "absolute"
            , "top" ~:: fromMaybe "0" topGap
            , "left" ~:: fromMaybe "0" leftGap
            , "z-index" ~:: maybe "1000" T.show zIndex
            , case (isVisible, hiddenOrNone) of
              (True, _) -> mconcat ["visibility" ~:: "visible", "display" ~:: "block"]
              (False, True) -> "visibility" ~:: "hidden"
              (False, False) -> "display" ~:: "none"
            ]
          )) : _popupConfig_interiorAttrs cfg
  elAttrs "div" attrsExterior $ do
    (el,a) <- elAttrs' "div" attrsInterior $ do
      widget
    let pressedEsc :: Event t ()
        pressedEsc = keydown Escape el
    pure (pressedEsc, a)

-- Default gap between popup container and interior. 8px (2 * 4px) is a common base spacing.
defaultPopupGap :: T.Text
defaultPopupGap = "8px"

setTopOffset :: Reflex t => Dynamic t (Maybe T.Text) -> PopupConfig t m -> PopupConfig t m
setTopOffset top cfg = cfg { _popupConfig_topGapFromContainer = top }

setLeftOffset :: Reflex t => Dynamic t (Maybe T.Text) -> PopupConfig t m -> PopupConfig t m
setLeftOffset left cfg = cfg { _popupConfig_leftGapFromContainer = left }

pBelow :: Reflex t => PopupConfig t m -> PopupConfig t m
pBelow = pBelowWithGap $ pure defaultPopupGap

pBelowWithGap :: Reflex t => Dynamic t T.Text -> PopupConfig t m -> PopupConfig t m
pBelowWithGap gapD = setTopOffset (fmap Just calcedBelow)
  where
    calcedBelow = ffor gapD $ \gap -> T.concat ["calc(100% + ", gap, ")"]

pAbove :: Reflex t => PopupConfig t m -> PopupConfig t m
pAbove = pAboveWithGap $ pure defaultPopupGap

pAboveWithGap :: Reflex t => Dynamic t T.Text -> PopupConfig t m -> PopupConfig t m
pAboveWithGap gapD = setTopOffset (fmap Just calcedAbove)
  where
    calcedAbove = ffor gapD $ \gap -> T.concat ["calc(-100% - ", gap, ")"]

pRight :: Reflex t => PopupConfig t m -> PopupConfig t m
pRight = pRightWithGap $ pure defaultPopupGap

pRightWithGap :: Reflex t => Dynamic t T.Text -> PopupConfig t m -> PopupConfig t m
pRightWithGap gapD = setLeftOffset (fmap Just calcedRight)
  where
    calcedRight = ffor gapD $ \gap -> T.concat ["calc(100% + ", gap, ")"]

pLeft :: Reflex t => PopupConfig t m -> PopupConfig t m
pLeft = pLeftWithGap $ pure defaultPopupGap

pLeftWithGap :: Reflex t => Dynamic t T.Text -> PopupConfig t m -> PopupConfig t m
pLeftWithGap gapD = setLeftOffset (fmap Just calcedLeft)
  where
    calcedLeft = ffor gapD $ \gap -> T.concat ["calc(-100% - ", gap, ")"]

pBottomLeft :: Reflex t => PopupConfig t m -> PopupConfig t m
pBottomLeft = pBottomLeftWithGap $ pure defaultPopupGap

pBottomLeftWithGap :: Reflex t => Dynamic t T.Text -> PopupConfig t m -> PopupConfig t m
pBottomLeftWithGap gapD = pLeftWithGap gapD . pBelowWithGap gapD

pBottomRight :: Reflex t => PopupConfig t m -> PopupConfig t m
pBottomRight = pBottomRightWithGap $ pure defaultPopupGap

pBottomRightWithGap :: Reflex t => Dynamic t T.Text -> PopupConfig t m -> PopupConfig t m
pBottomRightWithGap gapD = pRightWithGap gapD . pBelowWithGap gapD

pTopLeft :: Reflex t => PopupConfig t m -> PopupConfig t m
pTopLeft = pTopLeftWithGap $ pure defaultPopupGap

pTopLeftWithGap :: Reflex t => Dynamic t T.Text -> PopupConfig t m -> PopupConfig t m
pTopLeftWithGap gapD = pLeftWithGap gapD . pAboveWithGap gapD

pTopRight :: Reflex t => PopupConfig t m -> PopupConfig t m
pTopRight = pTopRightWithGap $ pure defaultPopupGap

pTopRightWithGap :: Reflex t => Dynamic t T.Text -> PopupConfig t m -> PopupConfig t m
pTopRightWithGap gapD = pRightWithGap gapD . pAboveWithGap gapD

pCentered :: (Reflex t, Applicative m) => PopupConfig t m -> PopupConfig t m
pCentered cfg =
  setLeftOffset (pure (Just "50%")) -- Set top of popup to center of container
    . setTopOffset (pure (Just "50%")) -- Set leftmost of popup to center of container
    $ cfg { _popupConfig_interiorAttrs = centeredStyles : _popupConfig_interiorAttrs cfg }
  where
    centeredStyles = "style" ~: ["transform" ~:: "translate(-50%, -50%)"]
    -- Now shift the popup back another 50% of its own width and height to actually center it
    -- If we didn't do this, it'd have its topleft at the center, instead of actually being centered
