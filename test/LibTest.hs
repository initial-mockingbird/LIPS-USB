{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Data.Semigroup
import           Data.Text        (Text)
import           Data.Text        as Text
import qualified Data.Text.IO     as Text
import           Text.Printf


import Lexer.Lexer ( lexer )
import           Test.Tasty
import           Test.Tasty.Hspec

main :: IO ()
main = hspec specLexer

readExamples :: IO [(Text, Text)]
readExamples =
    mapM asPair =<< Text.lines <$> Text.readFile "test/lexerTestCases.csv"
    where
        asPair line =
            case Text.splitOn ", " line of
            [input, expected] -> pure (input, expected)
            _ -> fail ("Invalid example line: " <> Text.unpack line)


specLexer :: Spec
specLexer = do
    examples <- runIO readExamples
    forM_ examples $ \(input, expected) ->
        it (printf "lexer '%s' to '%s'" input expected) $
        lexer (unpack input) `shouldBe` unpack expected