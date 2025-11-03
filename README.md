reflex-dom-popup
===============

Not actually a dropdown specific, but a popup. Use anywhere you normally use popups.

Usage
-----

Below is an example of using the popup, toggled by a simple button.

```haskell

> {-# OPTIONS_GHC -Werror=missing-fields #-}
> {-# Language OverloadedStrings #-}
> {-# Language RecursiveDo #-}
> {-# Language TypeApplications #-}
> {-# Language MultilineStrings #-}
> import Reflex.Dom
> import Reflex.Dom.Attrs
> import Reflex.Dom.Dropdown
> import Data.ByteString (ByteString)
> import Control.Monad.Fix (MonadFix)

> main :: IO ()
> main = mainWidgetWithCss extraStyle demoWidget

> demoWidget :: ( DomBuilder t m
>      , MonadFix m
>      , MonadHold t m
>      , PostBuild t m
>      , PerformEvent t m
>      , TriggerEvent t m
>      ) => m ()
> demoWidget = do
>   text "The dropdown below lets you toggle the visibility of the popup."
>   el "br" blank
>   buttonToggleE <- button "Click to toggle popup"
>   isVisibleD <- foldDyn (const not) False $ fmap (const True) buttonToggleE
>   popup
>       PopupConfig
>         { _popupConfig_visible = isVisibleD
>         , _popupConfig_hiddenOrNone = True
>         , _popupConfig_interiorAttrs = ["class" ~: ffor isVisibleD (\isVisible -> if isVisible then "popup-interior show" else "popup-interior")]
>         , _popupConfig_containerAttrs = ["style" ~: "color: blue"]
>         }
>       (do
>         text "Text inside popup")
>   el "br" blank
>   text "This is some text that immediately follows the popup, later in the page"
>   el "br" blank
>   text "This is some more text"
> 
> extraStyle :: ByteString
> extraStyle = """/* Add animation (fade in the popup) */
> @-webkit-keyframes fadeIn {
>   from {opacity: 0;}
>   to {opacity: 1;}
> }
> 
> @keyframes fadeIn {
>   from {opacity: 0;}
>   to {opacity:1 ;}
> }
> .popup-interior.show {
>   -webkit-animation: fadeIn 1s;
>   animation: fadeIn 1s;
> }
>"""


```


Hacking
-------

To work on this library, enter the nix shell with the following command:

```bash
nix-shell -A project.haskell-nix
```

Once you're inside that shell, you can use `cabal repl` for quick feedback
while developing. To build and test your changes, run:

```bash
javascript-unknown-ghcjs-cabal build
```

You'll see some output like this:

```
dist-newstyle/build/javascript-ghcjs/ghc-9.12.2/reflex-dom-lazy-0.1.0.0/x/reflex-dom-lazy/build/reflex-dom-lazy/reflex-dom-lazy.jsexe
```

Open `index.html` at that path to run the code from `Readme.lhs` in your browser.
