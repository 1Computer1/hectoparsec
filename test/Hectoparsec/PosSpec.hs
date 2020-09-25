module Hectoparsec.PosSpec (spec) where

import Test.Hspec

import Hectoparsec.Pos

spec :: Spec
spec = do
    describe "initialPos" $ do
        it "should have the right values" $
            initialPos "file" `shouldBe` Pos { posFile = "file", posLine = 1, posColumn = 1 }

    describe "updatePos" $ do
        it "should increment column for non-newlines" $
            updatePos (== '\n') 'x' (Pos "file" 1 10) `shouldBe` (Pos "file" 1 11)

        it "should increment line and set column to 1 for newlines" $
            updatePos (== '\n') '\n' (Pos "file" 1 10) `shouldBe` (Pos "file" 2 1)
