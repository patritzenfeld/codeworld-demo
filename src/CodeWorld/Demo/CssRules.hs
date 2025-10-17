{-# LANGUAGE OverloadedStrings #-}

module CodeWorld.Demo.CssRules (
  displayOnHover,
  noResize,
  verticalScroll,
  ) where


import Data.Text                        (Text)
import Web.Atomic.CSS (
  CSS,
  Declaration((:.)),
  Styleable,
  css,
  utility,
  )
import Web.Atomic.Types                 (className, selector)



verticalScroll :: Styleable h => CSS h -> CSS h
verticalScroll = utility "v-scroll" ["overflow-y" :. "auto"]


noResize :: Styleable h => CSS h -> CSS h
noResize = utility "no-resize" ["resize" :. "none"]


displayOnHover :: Styleable h => Text -> CSS h -> CSS h
displayOnHover parentClass = css
    childClass
    (selector (className parentClass) <> ":hover" <> " " <> selector childClass)
    [ "visibility" :. "visible"
    ]
  where
    childClass = "hover-reveal"
