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


import Control.Monad                    (void)
import Control.Monad.Except             (ExceptT, runExceptT, throwError)
import Control.Monad.Writer             (WriterT, runWriterT, tell)
import Data.Text                        (Text, pack, unpack)
import Effectful                        (IOE, MonadIO, liftIO)
import Haskell.Template.Task            (grade)
import System.FilePath
import System.Directory                 (getTemporaryDirectory)
import Text.PrettyPrint.Leijen.Text     (Doc)
import Web.Hyperbole
import Web.Atomic.CSS
import Web.Atomic.CSS.Layout

import CodeWorld.Demo.Style



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


data Submission f = Submission
  { config :: Field f Text
  , program :: Field f Text
  }
  deriving (Generic, FromFormF, GenFields FieldName, GenFields Validated)


data Feedback = Feedback
  deriving (Generic, ViewId)

data HoverMenu = HoverMenu
  deriving (Generic, ViewId)

data Reject = Reject deriving Show
type Output = ExceptT Reject (WriterT Doc IO)


reject :: Doc -> Output a
reject doc = tell doc >> throwError Reject


evaluate :: Output a -> IO (Either Reject a, Doc)
evaluate = runWriterT . runExceptT


suggest :: Doc -> Output ()
suggest = void . tell


instance IOE :> es => HyperView Feedback es where
  data Action Feedback
    = Submit
    | Load FilePath
    deriving (Generic, ViewAction)
  update (Load path) = do
    config <- loadConfig path
    program <- loadTask path
    let submission = Submission {config, program}
    pure $ tAreaForm submission "" (Editor, DefaultText)

  update Submit = do
    f <- formData @(Submission Identity)
    (status, doc) <- liftIO $ do
      tmp <- getTemporaryDirectory
      grade
        evaluate
        reject
        suggest
        tmp
        (unpack $ config f)
        (unpack $ program f)
    let feedback = show doc
    let colors = case (status, feedback) of
          (Left Reject, _) -> (FeedbackRejected,FeedbackRejectedText)
          (_, [])          -> (FeedbackOkay, DefaultText)
          _                -> (FeedbackSuggestion, FeedbackSuggestionText)
    pure $ tAreaForm f feedback colors


instance HyperView HoverMenu es where
  data Action HoverMenu
    = TriggerLoad
    deriving (Generic, ViewAction)
  type Require HoverMenu = '[Feedback]
  update TriggerLoad = pure hoverMenu


tAreaForm :: Submission Identity -> String -> (AppColor,AppColor) -> View Feedback ()
tAreaForm contents feedback (bgColor, textColor) = form Submit ~ grow $ do
  let f = fieldNames @Submission
  row ~ grow . pad 10 . gap 10 ~ bg Background $ do
    col ~ grow $ do
      field (program f) $ do
        header "Code Input"
        textarea (Just $ program contents) ~ bg Editor . grow @ value (program contents)
      submit "Submit" ~ bg UIElem . color UIText
    col ~ grow . maxWidth (Pct 0.43) $ do
      field (config f) $ do
        header "Config"
        textarea (Just $ config contents) ~ bg Editor . grow @ value (config contents)
      header "Feedback"
      el (text $ pack feedback) ~ bg bgColor . color textColor . grow . whiteSpace PreWrap . maxHeight (Pct 0.25) . verticalScroll


hoverMenu :: View HoverMenu ()
hoverMenu = el ~ stack @ class_ "dropdown-examples" $ do
  buttonMock "Load Examples"
  ol ~ popup (TL 20 10) . visibility Hidden . displayOnHover $ do
    let numerals = list Decimal
    li ~ numerals $ target Feedback $ button (Load "Task01") "Task01"
    li ~ numerals $ target Feedback $ button (Load "Task03") "Task03"
    li ~ numerals $ target Feedback $ button (Load "Task08") "Task08"


main :: IO ()
main = do
  run 3000 $ do
    liveApp quickStartDocument (runPage page)


page :: IOE :> es => Page es '[HoverMenu, Feedback]
page = do
  program <- loadTask "Task01"
  config <- loadConfig "Task01"
  pure $ do
    row ~ bg UIElem . color UIText $ do
      header "Title"
      space
      hyper HoverMenu hoverMenu
      space
      nav $ do
        link [uri|https://fmidue.github.io/codeworld-tasks/|] "Docs"
        link [uri|https://github.com/fmidue/codeworld-tasks|] "Repo"
    let submission = Submission {config, program}
    hyper Feedback (tAreaForm submission "" (Editor, DefaultText)) ~ display Flex . grow


buttonMock :: View c () -> View c ()
buttonMock = tag "button"


header :: Text -> View ctx ()
header txt = do
  el ~ bold $ text txt


loadFile :: MonadIO m => FilePath -> m Text
loadFile = liftIO . fmap pack . readFile . ("test-files" </>) . (<.> "hs")


loadConfig :: MonadIO m => FilePath -> m Text
loadConfig = loadFile . ("configs" </>)

loadTask :: MonadIO m => FilePath -> m Text
loadTask = loadFile . ("tasks" </>)
