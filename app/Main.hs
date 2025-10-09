{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Web.Hyperbole
import Web.Atomic.CSS
import Data.Text



main :: IO ()
main = do
  run 3000 $ do
    liveApp quickStartDocument (runPage page)


page :: Page es '[]
page = pure $ do
  row $ do
    header "Title"
    space
    el ~ stack $ do
      tag "button" "Load Example" @ class_ "dropdown-examples"
      ol ~ popup (TL 30 10) . display None . displayOnHover $ do
        let numerals = list Decimal
        li ~ numerals $ "one"
        li ~ numerals $ "two"
        li ~ numerals $ "three"
    space
    nav $ do
      link [uri|https://xd.org|] "Docs"
      link [uri|https://xd.org|] "Repo"
  row ~ pad 10 . gap 10 $ do
    col $ do
      header "Code Input"
      img "https://placehold.co/1200x800"
    col $ do
      header "Feedback"
      img "https://placehold.co/1200x800"
  header "run"


displayOnHover :: Styleable h => CSS h -> CSS h
displayOnHover = css
    ""
    ".dropdown-examples:hover + ol"
    [ "display" :. "block"
    ]



header :: Text -> View ctx ()
header txt = do
  el ~ bold $ text txt
