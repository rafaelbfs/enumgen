{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main (main) where

import           EnumGen.EnumParser
import           Test.Framework
import           Test.Hspec
import           Text.Parsec

main :: IO ()
main = hspec $ do
    describe "Enum Parsing" $ do
        it "parses enum declaration correctly" $
            parse enumParser "" "enum TestEnum Manual\n\
                \5:TestFive\n\
                \10:TestTen\n" `shouldBe` Right EnumAR{decl = EnumDecl{declName = "TestEnum", idType = Manual }, items = [EItem 5 "TestFive", EItem 10 "TestTen" ]}
