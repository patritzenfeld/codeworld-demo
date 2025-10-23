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
  Feedback(..),
  VisibleSection(..),
  externalNav,
  heading,
  hoverMenu,
  readConfig,
  tAreaForm,
  )
import Network.Wai.Middleware.Static (staticPolicy, addBase)


main :: IO ()
main = run 3000 $ staticPolicy (addBase "static") $ liveApp quickStartDocument $ runPage page


page :: (IOE :> es, Hyperbole :> es) => Page es '[Feedback]
page = do
  pageTitle "CodeWorld Tasks Demo"
  let configSection = Tests
  setParam "visible" configSection
  paths <- availableTasks
  confSegments <- case listToMaybe paths of
    Nothing -> pure ["","",""]
    Just path -> loadPreset path
  submission <- readConfig confSegments
  pure $ do
    script "buttons.js"
    row ~ uiStyle $ do
      heading "CodeWorld Tasks Demo"
      space
      hoverMenu paths
      space
      externalNav
    hyper Feedback (tAreaForm submission "" (Editor, UIText) configSection) ~ display Flex . grow
