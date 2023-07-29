{-# LANGUAGE OverloadedStrings #-}

module ProtocolSpec (spec) where

import Protocol (Msg(..), Body(..))
import Test.Hspec
import Data.Aeson (encode, decode)

spec :: Spec
spec = do
  describe "Msg JSON serialization/deserialization" $ do
    it "encodes and decodes Msg" $ do
      let msg = Msg "src_val" "dest_val" (Just 42) (Just 123) (Echo "echo_val")
      let encoded = encode msg
      let decoded = decode encoded :: Maybe Msg
      decoded `shouldBe` Just msg

  describe "Body JSON serialization/deserialization" $ do
    it "encodes and decodes Init Body" $ do
      let body = Init "nodeId_val" ["nodeId1", "nodeId2"]
      let encoded = encode body
      let decoded = decode encoded :: Maybe Body
      decoded `shouldBe` Just body

    it "encodes and decodes Echo Body" $ do
      let body = Echo "echo_val"
      let encoded = encode body
      let decoded = decode encoded :: Maybe Body
      decoded `shouldBe` Just body

    it "encodes and decodes EchoOk Body" $ do
      let body = EchoOk "echo_ok_val"
      let encoded = encode body
      let decoded = decode encoded :: Maybe Body
      decoded `shouldBe` Just body
