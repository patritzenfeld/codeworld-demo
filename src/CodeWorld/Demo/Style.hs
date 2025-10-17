{-# LANGUAGE OverloadedStrings #-}

module CodeWorld.Demo.Style (
  AppColor(..),
  anchorStyle,
  feedbackStyle,
  hoverMenuStyle,
  listStyle,
  popupStyle,
  textAreaStyle,
  uiStyle,
) where


import Data.Text                        (Text)
import Web.Atomic.CSS
import Web.Atomic.CSS.Layout            (maxHeight)

import CodeWorld.Demo.CssRules



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
  colorValue UIElem = "#800000" -- darker red
  colorValue FeedbackOkay = "#008000" -- green
  colorValue FeedbackRejected = "#842b26" -- dark red
  colorValue FeedbackRejectedText = "#FFF" -- white
  colorValue FeedbackSuggestion = "#FF8A00" -- orange
  colorValue FeedbackSuggestionText = "#000" -- black


textAreaStyle :: Styleable a => CSS a -> CSS a
textAreaStyle = bg Editor . grow


feedbackStyle :: Styleable a => AppColor -> AppColor -> CSS a -> CSS a
feedbackStyle bgColor textColor =
  grow . whiteSpace PreWrap . maxHeight (Pct 0.25) .
  verticalScroll . bg bgColor . color textColor


uiStyle :: Styleable a => CSS a -> CSS a
uiStyle = bg UIElem . color UIText . bold . textAlign AlignCenter . pad 10


anchorStyle :: Styleable a => CSS a -> CSS a
anchorStyle = pad 10


popupStyle :: Styleable a => Text -> Sides Length -> CSS a -> CSS a
popupStyle parent size = popup size . visibility Hidden . displayOnHover parent  . width (Pct 1)


listStyle :: Styleable a => CSS a -> CSS a
listStyle = uiStyle ~ border 1


hoverMenuStyle :: Styleable a => CSS a -> CSS a
hoverMenuStyle = stack . minWidth (PxRem 5)
