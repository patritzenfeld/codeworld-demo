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
    config <- loadFile $ path <.> "conf"
    program <- loadFile $ path <.> "hs"
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


tAreaForm :: Submission Identity -> String -> (AppColor,AppColor) -> View Feedback ()
tAreaForm contents feedback (bgColor, textColor) = form Submit ~ grow $ do
  let f = fieldNames @Submission
  row ~ grow . pad 10 . gap 10 ~ bg Background $ do
    col ~ grow $ do
      field (program f) $ do
        header "Code Input"
        textarea (Just $ program contents) ~ bg Editor . grow
      submit "Submit" ~ bg UIElem . color UIText
    col ~ grow . maxWidth (Pct 0.43) $ do
      field (config f) $ do
        header "Config"
        textarea (Just $ config contents) ~ bg Editor . grow
      header "Feedback"
      el (text $ pack feedback) ~ bg bgColor . color textColor . grow . whiteSpace PreWrap . maxHeight (Pct 0.15)


main :: IO ()
main = do
  run 3000 $ do
    liveApp quickStartDocument (runPage page)


page :: IOE :> es => Page es '[Feedback]
page = do
  program <- loadFile "test-files/Task01.hs"
  config <- loadFile "test-files/Task01.conf"
  pure $ do
    row ~ bg UIElem . color UIText $ do
      header "Title"
      space
      el ~ stack @ class_ "dropdown-examples" $ do
        buttonMock "Load Example"
        ol ~ popup (TL 20 10) . visibility Hidden . displayOnHover $ do
          let numerals = list Decimal
          li ~ numerals $ "Task01"
          li ~ numerals $ "Task03"
          li ~ numerals $ "Task08"
      space
      nav $ do
        link [uri|https://fmidue.github.io/codeworld-tasks/|] "Docs"
        link [uri|https://github.com/fmidue/codeworld-tasks|] "Repo"
    let submission = Submission {config, program}
    hyper Feedback (tAreaForm submission "" (Editor, DefaultText)) ~ display Flex . grow


buttonMock :: View c () -> View c ()
buttonMock = tag "button"

displayOnHover :: Styleable h => CSS h -> CSS h
displayOnHover = css
    ""
    ".dropdown-examples:hover ol"
    [ "visibility" :. "visible"
    ]


header :: Text -> View ctx ()
header txt = do
  el ~ bold $ text txt


loadFile :: MonadIO m => FilePath -> m Text
loadFile = liftIO . fmap pack . readFile
