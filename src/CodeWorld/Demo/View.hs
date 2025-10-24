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
  Feedback(..),
  Submission(..),
  VisibleSection(..),
  header,
  readConfig,
  tAreaForm,
) where


import qualified Data.Text               as T

import Data.Maybe                       (fromMaybe)
import Data.String                      (fromString)
import Data.Text                        (Text)
import Effectful                        (IOE)
import Web.Hyperbole.HyperView.Forms    (Input)
import Web.Hyperbole
import Web.Atomic.CSS (
  (~),
  fontSize,
  gap,
  pad,
  grow,
  bold,
  stack,
  visibility,
  Length(Pct),
  Sides(T),
  Visibility(..),
  )
import Web.Atomic.Attributes            (Attributable, Attributes)
import Web.Atomic.CSS.Layout            (maxWidth)

import CodeWorld.Demo.Server            (loadPreset, gradeSubmission)
import CodeWorld.Demo.Style (
  AppColor(..),
  mainSectionStyle,
  navbarStyle,
  textAreaStyle,
  feedbackStyle,
  uiStyle,
  anchorStyle,
  popupStyle,
  listStyle,
  hoverMenuStyle,
  buttonStyle,
  submitStyle,
  menuButtonStyle,
  )



data VisibleSection
  = Settings
  | Template
  | Tests
  deriving (Generic, FromParam, ToParam)


data Submission f = Submission
  { settings :: Field f Text
  , template :: Field f Text
  , tests :: Field f Text
  , program :: Field f Text
  }
  deriving (Generic, FromFormF, GenFields FieldName, GenFields Validated)


data Feedback = Feedback
  deriving (Generic, ViewId)


instance IOE :> es => HyperView Feedback es where
  data Action Feedback
    = Submit
    | Load FilePath
    deriving (Generic, ViewAction)
  update (Load path) = do
    confSegments <- loadPreset path
    submission <- readConfig confSegments
    vis <- fromMaybe Tests <$> lookupParam "visible"
    pure $ tAreaForm submission "" (Editor, UIText) vis

  update Submit = do
    f <- formData @(Submission Identity)
    let config = T.intercalate "---\n" [settings f, template f, tests f]
    (colors, feedback) <- gradeSubmission config (program f)
    vis <- fromMaybe Tests <$> lookupParam "visible"
    pure $ tAreaForm f feedback colors vis


externalNav :: View a ()
externalNav = nav ~ navbarStyle $ do
  anchor "Docs" [uri|https://fmidue.github.io/codeworld-tasks/|]
  anchor "Repo" [uri|https://github.com/fmidue/codeworld-tasks|]


anchor :: View c () -> URI -> View c ()
anchor = flip (link @ newPage . noReferrer ~ anchorStyle)
  where
    newPage = att "target" "_blank"
    noReferrer = att "rel" "noopener noreferrer"


header :: [FilePath] -> View (Root '[Feedback]) ()
header paths = row ~ uiStyle $ do
  heading "CodeWorld Tasks Demo" ~ pad 10
  space
  hoverMenu paths
  space
  externalNav


tAreaForm :: Submission Identity -> String -> (AppColor,AppColor) -> VisibleSection -> View Feedback ()
tAreaForm contents feedback (bgColor, textColor) visible = form Submit ~ grow $ do
  let f = fieldNames @Submission
  row ~ mainSectionStyle $ do
    col ~ grow $ do
      field (program f) $ do
        heading "Code Input"
        filledTextarea (program contents)
      submit "Submit" ~ submitStyle
    col ~ grow . maxWidth (Pct 0.43) $ do
      row ~ gap 10 $ do
        heading "Config"
        space
        jsButton "showSettings()" "Edit Settings" ~ buttonStyle
        jsButton "showTemplate()" "Edit Task Template" ~ buttonStyle
        jsButton "showTests()" "Edit Tests" ~ buttonStyle
      el ~ stack . grow $ do
        field (settings f) $ do
          filledTextarea ~ visSettings @ idAttr "settingsArea" $ settings contents
        field (template f) $ do
          filledTextarea ~ visTemplate @ idAttr "templateArea"$ template contents
        field (tests f) $ do
          filledTextarea ~ visTests @ idAttr "testArea" $ tests contents
      heading "Feedback"
      el (fromString feedback) ~ feedbackStyle bgColor textColor
  where
    (visSettings, visTemplate, visTests) = all3 visibility $ case visible of
      Settings -> (Visible, Hidden, Hidden)
      Template -> (Hidden, Visible, Hidden)
      Tests -> (Hidden, Hidden, Visible)
    all3 f (a,b,c) = (f a, f b, f c)


hoverMenu :: [FilePath] -> View (Root '[Feedback]) ()
hoverMenu paths = do
  let parentClass = "dropdown-examples"
  el ~ hoverMenuStyle @ class_ parentClass $ do
    heading "Load Examples" ~ menuButtonStyle
    ol ~ popupStyle parentClass (T 28) . uiStyle $
      mapM_ (li . target Feedback . liftA2 (button ~ listStyle) Load fromString) paths


heading :: Text -> View a ()
heading = (el ~ bold) . text


idAttr :: Attributable h => AttValue -> Attributes h -> Attributes h
idAttr = att "id"


jsButton :: Text -> View c () -> View c ()
jsButton event = (tag @ type_ "button" . att "onclick" event) "button"


filledTextarea :: Text -> View (Input id a) ()
filledTextarea contents = textarea (Just contents) ~ textAreaStyle @ value contents


readConfig :: Hyperbole :> es => [Text] -> Eff es (Submission Identity)
readConfig segments = case segments of
  [settings, template, tests] -> do
    pure Submission {settings, template, tests, program = template}
  _ -> respondErrorView "Error: Example template could not be read" $ do
    h1 "Internal Error"
    p "Example template could not be read. Please contact the maintainer!"
  where
    h1 = tag "h1" ~ fontSize 50 ~ bold
    p = tag "p"
