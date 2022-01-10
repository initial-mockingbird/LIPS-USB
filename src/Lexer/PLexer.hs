module Lexer.PLexer where

import Lexer.Lexer
import Text.Parsec
import Data.Functor (void, ($>), (<$))
import Control.Monad (when, mzero, unless)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Control.Monad.Trans (lift)
import Text.Parsec.Error 
import Control.Arrow 

digits :: [Char]
digits = ['0'..'9']

lowerCaseLetters :: [Char]
lowerCaseLetters = ['a'..'z']

upperCaseLetters :: [Char]
upperCaseLetters = ['A'..'Z']


operators :: Map String Token
operators = Map.fromList 
    [("(", TkOpenPar)
    ,(")",TkClosePar)
    ,("^",TkPower)
    ,("+",TkPlus)
    ,("-",TkMinus)
    ,("!",TkNot)
    ,("*",TkMult)
    ,("%",TkMod)
    ,("<",TkLT)
    ,("<=",TkLE)
    ,(">=",TkGE)
    ,(">",TkGT)
    ,("=",TkEQ)
    ,("&&",TkAnd)
    ,("||",TkOr)
    ,("<>",TkNE)
    ,("'",TkQuote)
    ,(",",TkComma)
    ,(":=",TkAssign)
    ,(";",TkSemicolon)
    ,("=>",TkYields)
    ,("->",TkRArrow)
    ,("<-",TkLArrow)
    ,("[",TkOpenBracket)
    ,("]",TkCloseBracket)
    ,("{",TkOpenBrace)
    ,("}",TkCloseBrace)
    ,(".",TkDot)
    ,(":",TkColon)
    ,("::",TkColonColon)
    ]

reservedWords :: Map String Token
reservedWords = Map.fromList 
    [ ("int",TkInt)
    , ("bool",TkBool)
    , ("type",TkType)
    , ("false",TkFalse)
    , ("true",TkTrue)
    , ("lazy",TkLazy)
    , ("while",TkWhile)
    , ("if" ,TkIf)
    , ("fun",TkFun)
    ] 

errorIdentifier :: String 
errorIdentifier = "Invalid identifier: ALL identifiers MUST begin with a letter and then be followed by a combination of letters, numbers and _"

parseIdentifier :: Monad m => ParsecT String u m Token  
parseIdentifier = spaces >> (\c cs -> TkId (c:cs)) <$> pFC <*> pRest  <?> errorIdentifier
    where
        validFirst = lowerCaseLetters ++ upperCaseLetters
        validChars = '_' : validFirst ++ digits
        pFC        = oneOf validFirst
        pRest      = many $ oneOf validChars

overFlowFail :: MonadFail m => String -> m a
overFlowFail s = fail $ "Overflown!: '" ++ s ++ "' Does not fit on a 32-bit integer!"

parseNumber :: Monad m => ParsecT String u m Token
parseNumber =  many1 (oneOf digits) >>= (\s -> if not $ checkOverflow s then overFlowFail s else return . TkNum . read $ s) 

invalidOperatorFail :: MonadFail m => String -> m a
invalidOperatorFail s = fail $ "Invalid Operator!: " ++ s

parseOP :: Monad m => ParsecT String u m Token 
parseOP = buildOp >>= (\s -> return (s, lookupOp s))  >>=  (\(s,s') -> maybe (invalidOperatorFail s) return s' )
    where
        lookupOp = flip Map.lookup operators
        buildOp = many (noneOf validChars)
        validFirst = lowerCaseLetters ++ upperCaseLetters
        validChars = ' ' : '_' : validFirst ++ digits



parseReserved :: Monad m => ParsecT String u m Token
parseReserved = buildOp >>= (\s ->  return (s, lookupReserved s))  >>=  (\(s,s') -> maybe (fail $ "The string: '" ++ s ++ "' is not a reserved word!") return s' )
    where
        lookupReserved = flip Map.lookup reservedWords
        buildOp = many lower 



lexer'' :: Monad m => ParsecT String u m [Token]
lexer'' = spaces >> manyTill (( parseNumber <|> parseOP  <|> try parseReserved <|> parseIdentifier) <* spaces) eof 

lexer' :: String -> IO ()
lexer' s = case runParser lexer'' () "." s of 
    Right tks -> putStrLn $ "OK: " ++ s ++ " ==> " ++ show tks
    Left pErr -> putStrLn $ "Error: " ++  s ++ " ==> " ++ last (map messageString (errorMessages pErr))