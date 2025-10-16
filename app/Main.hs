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


import qualified Data.Text              as T
import qualified Data.Text.IO           as T

import Control.Monad                    (void)
import Control.Monad.Except             (ExceptT, runExceptT, throwError)
import Control.Monad.Writer             (WriterT, runWriterT, tell)
import Data.List.Extra                  (sort, split)
import Data.Maybe                       (listToMaybe)
import Data.String                      (fromString)
import Data.Text                        (Text, unpack)
import Data.Tuple.Extra                 ((&&&))
import Effectful                        (IOE, MonadIO, liftIO)
import Haskell.Template.Task            (grade)
import System.FilePath                  ((</>))
import System.Directory                 (getTemporaryDirectory, listDirectory)
import Text.PrettyPrint.Leijen.Text     (Doc)
import Web.Hyperbole
import Web.Atomic.CSS
import Web.Atomic.CSS.Layout

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
    (config,program) <- loadPreset path
    let submission = Submission {config, program}
    pure $ tAreaForm submission "" (Editor, UIText)

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
          (_, [])          -> (FeedbackOkay, UIText)
          _                -> (FeedbackSuggestion, FeedbackSuggestionText)
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
  row ~ grow . pad 10 . gap 10 ~ bg Background $ do
    col ~ grow $ do
      field (program f) $ do
        header "Code Input"
        textarea (Just $ program contents) ~ textAreaStyle @ value (program contents)
      submit "Submit" ~ uiStyle
    col ~ grow . maxWidth (Pct 0.43) $ do
      field (config f) $ do
        header "Config"
        textarea (Just $ config contents) ~ textAreaStyle @ value (config contents)
      header "Feedback"
      el (fromString feedback) ~ feedbackStyle . bg bgColor . color textColor


hoverMenu :: [FilePath] -> View HoverMenu ()
hoverMenu paths = el ~ stack @ class_ "dropdown-examples" $ do
  header "Load Examples" ~ mousePointer
  ol ~ popup (T 20) . visibility Hidden . displayOnHover . uiStyle . width (Pct 1) $
    mapM_ ((li ~ uiStyle) . target Feedback . liftA2 button Load fromString) paths


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
      header "CodeWorld Tasks Demo"
      space
      hyper HoverMenu (hoverMenu paths) ~ minWidth (PxRem 5)
      space
      nav $ do
        link [uri|https://fmidue.github.io/codeworld-tasks/|] "Docs"
        link [uri|https://github.com/fmidue/codeworld-tasks|] "Repo"
    let submission = Submission {config, program}
    hyper Feedback (tAreaForm submission "" (Editor, UIText)) ~ display Flex . grow


header :: Text -> View ctx ()
header = (el ~ bold) . text


availableTasks :: MonadIO m => m [FilePath]
availableTasks = liftIO $ sort <$> listDirectory examplesDirectory


loadPreset :: MonadIO m => FilePath -> m (Text,Text)
loadPreset = liftIO .
  fmap (id &&& taskFromConfig) . T.readFile . (examplesDirectory </>)


splitConfig :: Text -> [Text]
splitConfig = map T.unlines . split ("---" `T.isPrefixOf`) . T.lines


taskFromConfig :: Text -> Text
taskFromConfig = (!! 1) . splitConfig


examplesDirectory :: FilePath
examplesDirectory = "templates"
