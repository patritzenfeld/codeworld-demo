{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module CodeWorld.Demo.View (
  ConfigPart(..),
  Feedback(..),
  Submission(..),
  externalNav,
  heading,
  hoverMenu,
  readConfig,
  tAreaForm,
) where


import qualified Data.Text               as T

import Data.String                      (fromString)
import Data.Text                        (Text)
import Effectful                        (IOE)
import Web.Hyperbole.HyperView.Forms    (Input)
import Web.Hyperbole
import Web.Atomic.CSS (
  (~),
  pointer,
  fontSize,
  grow,
  bold,
  stack,
  visibility,
  Length(Pct),
  Sides(T),
  Visibility(..),
  )
import Web.Atomic.CSS.Layout            (maxWidth)

import CodeWorld.Demo.Server            (loadPreset, gradeSubmission)
import CodeWorld.Demo.Style (
  noResize,
  AppColor(..),
  mainSectionStyle,
  textAreaStyle,
  feedbackStyle,
  uiStyle,
  anchorStyle,
  popupStyle,
  listStyle,
  hoverMenuStyle,
  )



data Submission f = Submission
  { settings :: Field f Text
  , template :: Field f Text
  , tests :: Field f Text
  , program :: Field f Text
  }
  deriving (Generic, FromFormF, GenFields FieldName, GenFields Validated)


data Feedback = Feedback
  deriving (Generic, ViewId)

data ConfigPart = Settings | Template | Tests
  deriving (Generic, ToParam, FromParam)


instance IOE :> es => HyperView Feedback es where
  data Action Feedback
    = Submit
    | Load FilePath
    | Open ConfigPart
    deriving (Generic, ViewAction)
  update (Load path) = do
    confSegments <- loadPreset path
    submission <- readConfig confSegments
    pure $ tAreaForm submission "" (Editor, UIText) Tests

  update Submit = do
    f <- formData @(Submission Identity)
    let config = T.intercalate "---\n" [settings f, template f, tests f]
    (colors, feedback) <- gradeSubmission config (program f)
    pure $ tAreaForm f feedback colors Tests

  update (Open cp) = do
    f <- formData @(Submission Identity)
    pure $ tAreaForm f "" (Editor, UIText) cp

externalNav :: View a ()
externalNav = nav $ do
  anchor "Docs" [uri|https://fmidue.github.io/codeworld-tasks/|]
  anchor "Repo" [uri|https://github.com/fmidue/codeworld-tasks|]


anchor :: View c () -> URI -> View c ()
anchor = flip link ~ anchorStyle


tAreaForm :: Submission Identity -> String -> (AppColor,AppColor) -> ConfigPart -> View Feedback ()
tAreaForm contents feedback (bgColor, textColor) focus = form Submit ~ grow $ do
  let f = fieldNames @Submission
  row ~ mainSectionStyle $ do
    col ~ grow $ do
      field (program f) $ do
        heading "Code Input"
        filledTextarea (program contents) ~ noResize
      submit "Submit" ~ uiStyle
    col ~ grow . maxWidth (Pct 0.43) $ do
      row $ do
        heading "Config"
        addContext Feedback $ do
          button (Open Settings) "Edit Settings" @ type_ "submit"
          button (Open Template) "Edit Task Template" @ type_ "submit"
          button (Open Tests) "Edit Tests" @ type_ "submit"
      el ~ stack . grow $ do
        field (settings f) $ do
          filledTextarea ~ a $ settings contents
        field (template f) $ do
          filledTextarea ~ b $ template contents
        field (tests f) $ do
          filledTextarea ~ c $ tests contents
      heading "Feedback"
      el (fromString feedback) ~ feedbackStyle bgColor textColor
  where
    vis = visibility Visible
    hidden = visibility Hidden
    (a,b,c) = case focus of
      Settings -> (vis,hidden,hidden)
      Template -> (hidden,vis,hidden)
      Tests    -> (hidden,hidden,vis)


hoverMenu :: [FilePath] -> View (Root '[Feedback]) ()
hoverMenu paths = do
  let parentClass = "dropdown-examples"
  el ~ hoverMenuStyle @ class_ parentClass $ do
    heading "Load Examples" ~ pointer
    ol ~ popupStyle parentClass (T 20) . uiStyle $
      mapM_ ((li ~ listStyle) . target Feedback . liftA2 button Load fromString) paths


heading :: Text -> View a ()
heading = (el ~ bold) . text


h1 :: View a () -> View a ()
h1 = tag "h1" ~ fontSize 50 ~ bold


p :: View a () -> View a ()
p = tag "p"


filledTextarea :: Text -> View (Input id a) ()
filledTextarea contents = textarea (Just contents) ~ textAreaStyle @ value contents


readConfig :: Hyperbole :> es => [Text] -> Eff es (Submission Identity)
readConfig segments = case segments of
  [settings, template, tests] -> do
    pure Submission {settings, template, tests, program = template}
  _ -> respondErrorView "Error: Example template could not be read" $ do
    h1 "Internal Error"
    p "Example template could not be read. Please contact the maintainer!"
