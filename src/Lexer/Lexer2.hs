{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{- |
Module      : Lexer
Description : Module that has all the lexer related functions.
Maintainer  : 15-11139@usb.ve, 16-10400@usb.ve 17-10538@usb.ve
Stability   : experimental
Portability : POSIX

Currently Holds the definition of the LIPS tokens, as well as the lexer function.
-}
module Lexer.Lexer2 where
import Text.ParserCombinators.Parsec hiding (token, tokens)
import Text.Read (readMaybe)
import Data.Bifunctor
import Text.Parsec.Error ( messageString, errorMessages  )
import Data.Foldable (traverse_)
import Debug.Trace
import Text.Parsec.Token (LanguageDef , GenTokenParser)
import qualified Text.Parsec.Token as L
import Data.Functor.Identity


-- | Tokens of the language
data Token
    = TkId String    -- ^ identifier
    | TkNum Int      -- ^ 32 bit integer
    | TkTrue         -- ^ > True
    | TkFalse        -- ^ > False
    | TkOpenPar      -- ^ @ (  @
    | TkClosePar     -- ^ @ ) @
    | TkPower        -- ^ @ ^ @
    | TkPlus         -- ^ @ + @
    | TkMinus        -- ^ @ - @
    | TkNot          -- ^ @ ! @
    | TkMult         -- ^ @ * @
    | TkMod          -- ^ Mod
    | TkLT           -- ^ @ < @
    | TkLE           -- ^ @ <= @
    | TkGE           -- ^ @ >= @
    | TkGT           -- ^ @ > @
    | TkEQ           -- ^ @ = @
    | TkNE           -- ^ @ <> @
    | TkAnd          -- ^ @ && @
    | TkOr           -- ^ @ || @
    | TkQuote        -- ^ @ ' @
    | TkComma        -- ^ @ , @
    | TkAssign       -- ^ @ := @
    | TkSemicolon    -- ^ @ ; @
    | TkYields       -- ^ @ => @
    | TkRArrow       -- ^ @ -> @
    | TkLArrow       -- ^ @ <- @
    | TkOpenBracket  -- ^ @ [ @
    | TkCloseBracket -- ^ @ ] @
    | TkOpenBrace    -- ^ @ {  @
    | TkCloseBrace   -- ^ @ } @
    | TkDot          -- ^ @ . @
    | TkColon        -- ^ @ : @
    | TkColonColon   -- ^ @ :: @
    | TkWhile        -- ^ @ While @
    | TkIf           -- ^ @ If @
    deriving (Show,Eq)

-- | Each lex line should save the token and the position, in case of errors.
type TokenPos =  (Token,SourcePos)


language :: LanguageDef ()
language = L.LanguageDef 
    ""
    ""
    ""
    False
    (letter <|> char '_')
    (alphaNum <|> char '_')
    (oneOf "^+-!*<=>|:.&")
    (oneOf "^+-!*<=>|:.&")
    ["while","if","mod","True","False","Mod"]
    ["<>","<=",">=","||","&&",":","::",":=","="]
    True 


genLexerParser :: GenTokenParser String () Identity
genLexerParser = L.makeTokenParser language

identifier = L.identifier genLexerParser
lexeme     = L.lexeme genLexerParser
whiteSpace = L.whiteSpace genLexerParser


tkID :: Parser TokenPos
tkID = do
    i      <- getInput
    pos    <- getPosition 
    cosito <- lexeme identifier <?> ("ERROR: lexer(" ++ show i ++ ") ==> Inicializador de identificador invalido")
    return (TkId cosito,pos) 

{-
Lexer:

<sentence> 
    = <operator>
    | <brackets>
    | <number>
    | <reserved>
    | <identifier>

<brackets>   = ()[]
<operator>   = TO MANY TO LIST, but you get the gist
<number>     = 32 bit number
<reserved>   = While, If, True, False, Mod
<identifier> = any string that starts with: [A-Z] + [a-z] + [_] and
               ends with: ([A-Z] + [a-z] + [_] + [0-9])*

-}


showTokenPos :: String -> [TokenPos] -> String
showTokenPos input tokens = "OK: lexer(" ++  show input ++  ") ==> " ++ show (map fst tokens)
