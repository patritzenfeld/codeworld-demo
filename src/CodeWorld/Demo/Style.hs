{-# LANGUAGE OverloadedStrings #-}

module CodeWorld.Demo.Style (
  AppColor(..),
  anchorStyle,
  displayOnHover,
  feedbackStyle,
  listStyle,
  mousePointer,
  noResize,
  popupStyle,
  textAreaStyle,
  uiStyle,
  verticalScroll,
) where


import Web.Atomic.CSS
import Web.Atomic.CSS.Layout (maxHeight)



data AppColor
  = Editor
  | Background
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
  colorValue Editor = "#FFF" -- white
  colorValue Background = "#D3D3D3" -- light gray
  colorValue UIElem = "#800000" -- dark red
  colorValue FeedbackOkay = "#008000" -- green
  colorValue FeedbackRejected = "#842b26" -- bright red
  colorValue FeedbackRejectedText = "#FFF" -- white
  colorValue FeedbackSuggestion = "#FF8A00" -- orange
  colorValue FeedbackSuggestionText = "#000" -- black


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
uiStyle = bg UIElem . color UIText . bold . textAlign AlignCenter . pad 10


anchorStyle :: Styleable a => CSS a -> CSS a
anchorStyle = pad 10


popupStyle :: Styleable a => Sides Length -> CSS a -> CSS a
popupStyle size = popup size . visibility Hidden . displayOnHover . width (Pct 1)


listStyle :: Styleable a => CSS a -> CSS a
listStyle = uiStyle ~ border 1


noResize :: Styleable h => CSS h -> CSS h
noResize = utility "no-resize" ["resize" :. "none"]
