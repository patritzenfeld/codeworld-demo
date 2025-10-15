{-# LANGUAGE OverloadedStrings #-}

module CodeWorld.Demo.Style (
  displayOnHover,
  verticalScroll,
) where


import Web.Atomic.CSS (
  CSS,
  Declaration((:.)),
  Styleable,
  css,
  utility
  )



verticalScroll :: Styleable h => CSS h -> CSS h
verticalScroll = utility "v-scroll" ["overflow-y" :. "auto"]


displayOnHover :: Styleable h => CSS h -> CSS h
displayOnHover = css
    "hover-reveal"
    ".dropdown-examples:hover .hover-reveal"
    [ "visibility" :. "visible"
    ]
