{-# LANGUAGE OverloadedStrings #-}

module CodeWorld.Demo.Style (
  AppColor(..),
  anchorStyle,
  buttonStyle,
  feedbackStyle,
  hoverMenuStyle,
  mainSectionStyle,
  listStyle,
  popupStyle,
  submitStyle,
  textAreaStyle,
  uiStyle,
  -- re-exported
  noResize,
) where


import Data.Text                        (Text)
import Web.Atomic.CSS
import Web.Atomic.CSS.Layout            (maxHeight)

import CodeWorld.Demo.CssRules



data AppColor
  = Editor
  | Background
  | UIElem
  | UIElemHover
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
  colorValue UIElemHover = "#FF8A00"
  colorValue FeedbackOkay = "#008000" -- green
  colorValue FeedbackRejected = "#842b26" -- dark red
  colorValue FeedbackRejectedText = "#FFF" -- white
  colorValue FeedbackSuggestion = "#FF8A00" -- orange
  colorValue FeedbackSuggestionText = "#000" -- black


mainSectionStyle :: Styleable a => CSS a -> CSS a
mainSectionStyle = grow . pad 10 . gap 10 ~ bg Background


textAreaStyle :: Styleable a => CSS a -> CSS a
textAreaStyle = grow . bg Editor


feedbackStyle :: Styleable a => AppColor -> AppColor -> CSS a -> CSS a
feedbackStyle bgColor textColor =
  grow . whiteSpace PreWrap . maxHeight (Pct 0.25) .
  verticalScroll . bg bgColor . color textColor


uiStyle :: Styleable a => CSS a -> CSS a
uiStyle = uiShare . pad 10


submitStyle :: Styleable a => CSS a -> CSS a
submitStyle = uiStyle . clickable


uiClickable :: Styleable a => CSS a -> CSS a
uiClickable = uiShare . clickable


anchorStyle :: Styleable a => CSS a -> CSS a
anchorStyle = pad 10 . clickable


popupStyle :: Styleable a => Text -> Sides Length -> CSS a -> CSS a
popupStyle parent size = popup size . visibility Hidden . displayOnHover parent  . width (Pct 1)


listStyle :: Styleable a => CSS a -> CSS a
listStyle = uiClickable . border 1 . pad 10


hoverMenuStyle :: Styleable a => CSS a -> CSS a
hoverMenuStyle = stack . minWidth (PxRem 5)


buttonStyle :: Styleable a => CSS a -> CSS a
buttonStyle = uiClickable . border (X 1) . pad (X 5)


uiShare :: Styleable a => CSS a -> CSS a
uiShare = bg UIElem . color UIText . bold . textAlign AlignCenter


clickable :: Styleable a => CSS a -> CSS a
clickable = hover (bg UIElemHover)
