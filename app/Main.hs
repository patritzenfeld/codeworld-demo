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

module Main where

import Web.Hyperbole
import Web.Atomic.CSS
import Data.Text
import Effectful
import System.Directory
import System.Exit                      (exitFailure)
import Haskell.Template.Task            (grade)
import Text.PrettyPrint.Leijen.Text     (putDoc)


data AppColor
  = White
  | LightGray
  | DarkGray
  | DarkRed
  deriving (Show)


instance ToColor AppColor where
  colorValue White = "#FFF"
  colorValue LightGray = "#D3D3D3"
  colorValue DarkGray = "#626262"
  colorValue DarkRed = "#800000"



newtype Submission f = Submission
  { program :: Field f Text
  }
  deriving (Generic, FromFormF, GenFields FieldName, GenFields Validated)


data Feedback = Feedback
  deriving (Generic, ViewId)


instance IOE :> es => HyperView Feedback es where
  data Action Feedback
    = Submit
    deriving (Generic, ViewAction)
  update Submit = do
    f <- formData @(Submission Identity)
    liftIO $ do
      tmp <- getTemporaryDirectory
      task <- readFile "test-files/Task01.conf"
      grade
        id
        rejection
        suggestion
        tmp
        task
        (unpack $ program f)
    liftIO $ print @Int 2
    pure $ do
      tAreaForm f
    where
      rejection d = putDoc d >> exitFailure
      suggestion _ = putStrLn "suggestion"



tAreaForm :: Submission Identity -> View Feedback ()
tAreaForm contents = form Submit ~ grow $ do
  -- let f = fieldNames @Submission
  field "program" $ do
    header "Code Input"
    textarea (Just $ program contents) ~ bg LightGray . grow  -- . height (PxRem 500)
  submit "Submit" ~ bg DarkRed . color White



main :: IO ()
main = do
  run 3000 $ do
    liveApp quickStartDocument (runPage page)


page :: IOE :> es => Page es '[Feedback]
page = do
  template <- liftIO $ readFile "test-files/Task01.hs"
  pure $ do
    row ~ bg DarkRed . color White $ do
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
    row ~ grow . pad 10 . gap 10 ~ bg DarkGray $ do
      hyper Feedback (tAreaForm $ Submission $ pack template)~ display Flex . grow
      col ~ grow $ do
        header "Feedback"
        el "" ~ bg LightGray . grow



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
