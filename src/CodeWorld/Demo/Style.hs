{-# LANGUAGE OverloadedStrings #-}

module CodeWorld.Demo.Style (
  AppColor(..),
  displayOnHover,
  feedbackStyle,
  mousePointer,
  textAreaStyle,
  uiStyle,
  verticalScroll,
) where


import Web.Atomic.CSS (
  CSS,
  Declaration((:.)),
  Length(Pct),
  Styleable,
  ToColor(..),
  WhiteSpace(PreWrap),
  bg,
  bold,
  color,
  css,
  grow,
  whiteSpace,
  utility
  )
import Web.Atomic.CSS.Layout (maxHeight)



data AppColor
  = Editor
  | Background
  | DefaultText
  | UIElem
  | UIText
  | FeedbackOkay
  | FeedbackRejected
  | FeedbackRejectedText
  | FeedbackSuggestion
  | FeedbackSuggestionText
  deriving (Show)


instance ToColor AppColor where
  colorValue UIText = "#FFF" -- white
  colorValue Editor = "#D3D3D3" -- light gray
  colorValue Background = "#626262" -- dark gray
  colorValue UIElem = "#800000" -- dark red
  colorValue FeedbackOkay = "#008000" -- green
  colorValue FeedbackRejected = "#B32821" -- bright red
  colorValue FeedbackRejectedText = "#009ED4" -- light blue
  colorValue FeedbackSuggestion = "#FF8A00" -- orange
  colorValue FeedbackSuggestionText = "#4A00FF" -- navy blue
  colorValue DefaultText = "#000" -- black


mousePointer :: Styleable h => CSS h -> CSS h
mousePointer = utility "mouse-pointer" ["cursor" :. "pointer"]


verticalScroll :: Styleable h => CSS h -> CSS h
verticalScroll = utility "v-scroll" ["overflow-y" :. "auto"]


displayOnHover :: Styleable h => CSS h -> CSS h
displayOnHover = css
    "hover-reveal"
    ".dropdown-examples:hover .hover-reveal"
    [ "visibility" :. "visible"
    ]


textAreaStyle :: Styleable a => CSS a -> CSS a
textAreaStyle = bg Editor . grow


feedbackStyle :: Styleable a => CSS a -> CSS a
feedbackStyle = grow . whiteSpace PreWrap . maxHeight (Pct 0.25) . verticalScroll


uiStyle :: Styleable a => CSS a -> CSS a
uiStyle = bg UIElem . color UIText . bold
