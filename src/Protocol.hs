{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Protocol
  ( Msg (Msg),
    src,
    dest,
    msgId,
    inReplyTo,
    body,
    Body (Echo, EchoOk, Init, InitOk),
    echo,
    nodeId,
    nodeIds,
  )
where

import Data.Aeson (FromJSON (parseJSON), Key, ToJSON (toJSON))
import Data.Aeson.Extra (lodashMerge)
import Data.Aeson.Types (Object, Parser, Value (Object), object, (.:), (.:?), (.=))
import Data.Maybe (catMaybes)
import Data.Text qualified as T
import Deriving.Aeson
  ( CamelToSnake,
    ConstructorTagModifier,
    CustomJSON (CustomJSON),
    FieldLabelModifier,
    Generic,
    OmitNothingFields,
    SumTaggedObject,
  )

data Msg = Msg
  { src :: T.Text,
    dest :: T.Text,
    msgId :: Maybe Int,
    inReplyTo :: Maybe Int,
    body :: Body
  }
  deriving (Show, Eq)

instance ToJSON Msg where
  toJSON Msg {src, dest, msgId, inReplyTo, body} =
    object
      [ "src" .= src,
        "dest" .= dest,
        "body"
          .= lodashMerge
            (toJSON body)
            (object $ catMaybes [("msg_id" .=) <$> msgId, ("in_reply_to" .=) <$> inReplyTo])
      ]

(.->) :: (FromJSON a) => Parser Object -> Key -> Parser (Maybe a)
(.->) parser key = parser >>= (.:? key)

instance FromJSON Msg where
  parseJSON (Object obj) =
    Msg
      <$> obj .: "src"
      <*> obj .: "dest"
      <*> obj .: "body" .-> "msg_id"
      <*> obj .: "body" .-> "in_reply_to"
      <*> obj .: "body"
  parseJSON _ = fail "Failed to parse Msg"

data Body where
  Init :: {nodeId :: T.Text, nodeIds :: [T.Text]} -> Body
  InitOk :: Body
  Echo :: {echo :: T.Text} -> Body
  EchoOk :: {echo :: T.Text} -> Body
  deriving (Eq, Generic, Show)
  deriving
    (FromJSON, ToJSON)
    via CustomJSON
          '[ OmitNothingFields,
             FieldLabelModifier '[CamelToSnake],
             SumTaggedObject "type" "payload",
             ConstructorTagModifier CamelToSnake
           ]
          Body
