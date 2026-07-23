{-# LANGUAGE OverloadedStrings #-}

module CodeWorld.Demo.Server (
  availableTasks,
  gradeSubmission,
  loadPreset,
) where


import qualified Data.Text              as T
import qualified Data.Text.IO           as T

import Control.Monad                    (void)
import Control.Monad.Except (
  ExceptT,
  runExceptT,
  throwError,
  withExceptT,
  )
import Control.Monad.Writer             (WriterT, runWriterT, tell)
import Data.Char                        (toLower)
import Data.List.Extra                  (sort, split)
import Data.Text                        (Text, unpack)
import Effectful                        (MonadIO, liftIO)
import Haskell.Template.Task            (grade)
import Text.PrettyPrint.Leijen.Text     (Doc)
import System.Directory                 (getTemporaryDirectory, listDirectory)
import System.FilePath                  ((</>))

import CodeWorld.Demo.Style             (AppColor(..))


data Reject = Unknown | Syntax | Semantics deriving Show
type Output = ExceptT Reject (WriterT Doc IO)


reject :: Doc -> Output a
reject doc = tell doc >> throwError Unknown


evaluateSyntax :: Output a -> Output a
evaluateSyntax = replaceReason Syntax


evaluateSemantics :: Output a -> Output a
evaluateSemantics = replaceReason Semantics


replaceReason :: Reject -> Output a -> Output a
replaceReason = withExceptT . const


suggest :: Doc -> Output ()
suggest = void . tell


availableTasks :: MonadIO m => m [FilePath]
availableTasks = liftIO $ sort <$> listDirectory examplesDirectory


loadPreset :: MonadIO m => FilePath -> m [Text]
loadPreset = liftIO .
  fmap splitConfig . T.readFile . (examplesDirectory </>)


splitConfig :: Text -> [Text]
splitConfig = map T.unlines . split ("---" `T.isPrefixOf`) . T.lines


examplesDirectory :: FilePath
examplesDirectory = "templates"


gradeSubmission :: MonadIO m => Text -> Text -> m ((AppColor,AppColor), String)
gradeSubmission config program = liftIO $ do
  tmp <- getTemporaryDirectory
  (status, doc) <- runWriterT $ runExceptT $ grade
    evaluateSyntax
    evaluateSemantics
    reject
    suggest
    tmp
    (unpack config)
    (unpack program)
  let (colors,feedback) = case (status,show doc) of
        (Left reason, output) ->
          ( (FeedbackRejected, FeedbackRejectedText)
          , concat ["Rejected for ", map toLower $ show reason, ":\n\n", output]
          )
        (Right (), "")        -> ((FeedbackOkay, UIText), "Okay")
        (Right (), output)    -> ((FeedbackSuggestion, FeedbackSuggestionText), output)
  pure (colors, feedback)
