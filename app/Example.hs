{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Example where

import Data.Text
import Web.Hyperbole
import Web.Atomic.CSS
import Data.Maybe

main :: IO ()
main = do
  run 3000 $ do
    liveApp quickStartDocument (runPage page)




data Message = Message
  deriving (Generic, ViewId)


newtype TextRes = TextRes
  { t :: Text
  }
  deriving (Generic, FromForm)


newtype TextRes' f = TextRes'
  { te :: Field f Text
  }
  deriving (Generic, FromFormF, GenFields FieldName, GenFields Validated)


validateForm :: TextRes' Identity -> TextRes' Validated
validateForm u =
  TextRes'
    { te = validateText $ te u
    }

validateText :: Text -> Validated Text
validateText areaText = validate (areaText == "cat") "I hate cats!"


data AddText = AddText
  deriving (Generic, ViewId)

instance HyperView AddText es where
  data Action AddText
    = Submit
    deriving (Generic, ViewAction)
  update Submit = do
    f <- formData @(TextRes' Identity)
    let val = validateForm f
    pure $ do
      tAreaForm val
      el $ text $ "last submission: " <> te f



tAreaForm :: TextRes' Validated -> View AddText ()
tAreaForm result = form Submit $ do
  let f = fieldNames @TextRes'
  field (te f) $ do
    label $ do
      el $ invalidText $ te result
      textarea Nothing @ placeholder "enter text"
  submit "Submit"



page :: (Hyperbole :> es) => Page es '[Message,AddText]
page = do
  p <- lookupParam "msg"
  let msg = fromMaybe "hello" p
  pure $ do
    hyper AddText $ tAreaForm genFields

    hyper Message $ messageView msg

instance HyperView Message es where
  data Action Message
    = Louder Text
    deriving (Generic, ViewAction)

  update (Louder msg) = do
    let new = msg <> "!"
    setParam "msg" new
    pure $ messageView new

messageButton :: Text -> View Message ()
messageButton msg = do
  button (Louder msg) ~ border 1 $ text "louder"

messageView :: Text -> View Message ()
messageView msg = do
  header msg
  messageButton msg



header :: Text -> View ctx ()
header txt = do
  el ~ bold $ text txt
