{-# LANGUAGE OverloadedStrings #-}
module SimpleSpec where

import Data.Attoparsec.ByteString
import Data.ByteString.Search as BS
import Data.ByteString        as BS (ByteString)
import Test.Hspec
import Test.QuickCheck
import Parser
import Type
import Convert

spec :: Spec
spec = do
  describe "Parser.convertMD" $ do
    it "parses bold" $ do
      tConvertMD "**test**" `shouldBe` (Just "<p><strong>test</strong></p>")
    it "parses italic" $ do
      tConvertMD "*test*" `shouldBe` (Just "<p><em>test</em></p>")
    it "parses boldAndItalic" $ do
      tConvertMD "***test***" `shouldBe` (Just "<p><strong><em>test</em></strong></p>")

tConvertMD x = BS.replace "\n" (""::ByteString) <$> convertMD x
