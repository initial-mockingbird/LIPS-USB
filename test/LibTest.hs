{-# LANGUAGE OverloadedStrings #-}

module LibTest where

import           Control.Monad
import           Data.Semigroup
import           Data.Text        (Text)
import           Data.Text        as Text
import qualified Data.Text.IO     as Text
import           Text.Printf

import           Lib
import           Lexer.Lexer

import           Test.Tasty
import           Test.Tasty.Hspec

main :: IO ()
main = hspec spec_lexer

readExamples :: IO [(Text, Text)]
readExamples =
    mapM asPair =<< Text.lines <$> Text.readFile "test/lexerTestCases.csv"
    where
        asPair line =
            case Text.splitOn "," line of
            [input, expected] -> pure (input, expected)
            _ -> fail ("Invalid example line: " <> Text.unpack line)


specLexer :: Spec
specLexer = do
    examples <- runIO readExamples
    forM_ examples $ \(input, expected) ->
        it (printf "lexer '%s' to '%s'" input expected) $
        lexer input `shouldBe` expected