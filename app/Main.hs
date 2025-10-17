{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Main where


import Data.Maybe                       (listToMaybe)
import Effectful                        (IOE)
import Web.Hyperbole
import Web.Atomic.CSS (
  (~),
  display,
  grow,
  Display(Flex),
  )

import CodeWorld.Demo.Server (
  availableTasks,
  loadPreset,
  )
import CodeWorld.Demo.Style             (AppColor(..), uiStyle)
import CodeWorld.Demo.View (
  ConfigPart(..),
  Feedback(..),
  externalNav,
  heading,
  hoverMenu,
  readConfig,
  tAreaForm,
  )



main :: IO ()
main = run 3000 $ liveApp quickStartDocument $ runPage page


page :: (IOE :> es, Hyperbole :> es) => Page es '[Feedback]
page = do
  pageTitle "CodeWorld Tasks Demo"
  paths <- availableTasks
  confSegments <- case listToMaybe paths of
    Nothing -> pure ["","",""]
    Just path -> loadPreset path
  submission <- readConfig confSegments
  pure $ do
    row ~ uiStyle $ do
      heading "CodeWorld Tasks Demo"
      space
      hoverMenu paths
      space
      externalNav
    hyper Feedback (tAreaForm submission "" (Editor, UIText) Settings) ~ display Flex . grow
