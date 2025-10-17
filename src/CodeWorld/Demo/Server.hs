{-# LANGUAGE OverloadedStrings #-}

module CodeWorld.Demo.Server (
  availableTasks,
  gradeSubmission,
  loadPreset,
) where


import qualified Data.Text              as T
import qualified Data.Text.IO           as T

import Control.Monad                    (void)
import Control.Monad.Except             (ExceptT, runExceptT, throwError)
import Control.Monad.Writer             (WriterT, runWriterT, tell)
import Data.List.Extra                  (sort, split)
import Data.Text                        (Text, unpack)
import Data.Tuple.Extra                 ((&&&))
import Effectful                        (MonadIO, liftIO)
import Haskell.Template.Task            (grade)
import Text.PrettyPrint.Leijen.Text     (Doc)
import System.Directory                 (getTemporaryDirectory, listDirectory)
import System.FilePath                  ((</>))

import CodeWorld.Demo.Style             (AppColor(..))


data Reject = Reject deriving Show
type Output = ExceptT Reject (WriterT Doc IO)


reject :: Doc -> Output a
reject doc = tell doc >> throwError Reject


evaluate :: Output a -> IO (Either Reject a, Doc)
evaluate = runWriterT . runExceptT


suggest :: Doc -> Output ()
suggest = void . tell


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


gradeSubmission :: MonadIO m => Text -> Text -> m ((AppColor,AppColor), String)
gradeSubmission config program = liftIO $ do
  tmp <- getTemporaryDirectory
  (status, doc) <- grade
    evaluate
    reject
    suggest
    tmp
    (unpack config)
    (unpack program)
  let feedback = show doc
  let colors = case (status, feedback) of
        (Left Reject, _) -> (FeedbackRejected,FeedbackRejectedText)
        (_, [])          -> (FeedbackOkay, UIText)
        _                -> (FeedbackSuggestion, FeedbackSuggestionText)
  pure (colors, feedback)
