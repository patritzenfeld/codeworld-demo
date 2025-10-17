{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where


import Data.Maybe                       (listToMaybe)
import Data.String                      (fromString)
import Data.Text                        (Text)
import Effectful                        (IOE)
import Web.Hyperbole
import Web.Hyperbole.HyperView.Forms    (Input)
import Web.Atomic.CSS
import Web.Atomic.CSS.Layout

import CodeWorld.Demo.Server (
  availableTasks,
  gradeSubmission,
  loadPreset,
  )
import CodeWorld.Demo.Style



data Submission f = Submission
  { config :: Field f Text
  , program :: Field f Text
  }
  deriving (Generic, FromFormF, GenFields FieldName, GenFields Validated)


data Feedback = Feedback
  deriving (Generic, ViewId)

data HoverMenu = HoverMenu
  deriving (Generic, ViewId)


instance IOE :> es => HyperView Feedback es where
  data Action Feedback
    = Submit
    | Load FilePath
    deriving (Generic, ViewAction)
  update (Load path) = do
    (config,program) <- loadPreset path
    let submission = Submission {config, program}
    pure $ tAreaForm submission "" (Editor, UIText)

  update Submit = do
    f <- formData @(Submission Identity)
    (colors, feedback) <- gradeSubmission (config f) (program f)
    pure $ tAreaForm f feedback colors


instance HyperView HoverMenu es where
  data Action HoverMenu
    = TriggerLoad
    deriving (Generic, ViewAction)
  type Require HoverMenu = '[Feedback]
  -- never called. This has to be doable without a HyperView instance???
  update TriggerLoad = undefined


tAreaForm :: Submission Identity -> String -> (AppColor,AppColor) -> View Feedback ()
tAreaForm contents feedback (bgColor, textColor) = form Submit ~ grow $ do
  let f = fieldNames @Submission
  row ~ mainSectionStyle $ do
    col ~ grow $ do
      field (program f) $ do
        heading "Code Input"
        filledTextarea (program contents) ~ noResize
      submit "Submit" ~ uiStyle
    col ~ grow . maxWidth (Pct 0.43) $ do
      field (config f) $ do
        heading "Config"
        filledTextarea $ config contents
      heading "Feedback"
      el (fromString feedback) ~ feedbackStyle bgColor textColor


hoverMenu :: [FilePath] -> View HoverMenu ()
hoverMenu paths = do
  let parentClass = "dropdown-examples"
  el ~ hoverMenuStyle @ class_ parentClass $ do
    heading "Load Examples" ~ pointer
    ol ~ popupStyle parentClass (T 20) . uiStyle $
      mapM_ ((li ~ listStyle) . target Feedback . liftA2 button Load fromString) paths


main :: IO ()
main = run 3000 $ liveApp quickStartDocument $ runPage page


page :: IOE :> es => Page es '[HoverMenu, Feedback]
page = do
  paths <- availableTasks
  (config,program) <- case listToMaybe paths of
    Nothing -> pure ("","")
    Just path -> loadPreset path
  pure $ do
    row ~ uiStyle $ do
      heading "CodeWorld Tasks Demo"
      space
      hyper HoverMenu $ hoverMenu paths
      space
      nav $ do
        anchor "Docs" [uri|https://fmidue.github.io/codeworld-tasks/|]
        anchor "Repo" [uri|https://github.com/fmidue/codeworld-tasks|]
    let submission = Submission {config, program}
    hyper Feedback (tAreaForm submission "" (Editor, UIText)) ~ display Flex . grow


heading :: Text -> View a ()
heading = (el ~ bold) . text


anchor :: View c () -> URI -> View c ()
anchor = flip link ~ anchorStyle


filledTextarea :: Text -> View (Input id a) ()
filledTextarea contents = textarea (Just contents) ~ textAreaStyle @ value contents
