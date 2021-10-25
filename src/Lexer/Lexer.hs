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
module Lexer.Lexer where
import Text.ParserCombinators.Parsec hiding (token, tokens)
import Text.Read (readMaybe)
import Data.Bifunctor
import Text.Parsec.Error ( messageString, errorMessages  )
import Data.Foldable (traverse_)
import Debug.Trace

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


-- | Tokenizes an identifier
idTokenizer :: Parser TokenPos
idTokenizer = do
    pos <- getPosition
    i   <- getInput 
    fc  <- oneOf firstChar
    r   <- optionMaybe (many $ oneOf rest)
    spaces
    return $ (,pos) $ 
        case r of
            Nothing -> TkId [fc]
            Just s  -> TkId $ fc : s
  where firstChar = ['A'..'Z'] ++ ['a'..'z'] ++ "_"
        rest      = firstChar ++ ['0'..'9']

-- | Tokenizes a number.
numTokenizer :: Parser TokenPos
numTokenizer = do
    i      <- getInput  
    pos    <- getPosition 
    number <- readMaybe <$> many digit
    pos'    <- getPosition
    let i'' = take (sourceColumn pos') i
    notFollowedBy idTokenizer <?> ("ERROR: lexer(" ++ show i'' ++ ") ==> Inicializador de identificador invalido")
    spaces
    case number of
        Nothing -> do
            i <- getInput
            fail $ "ERROR: lexer(" ++ show i ++  ") ==> Overflow: entero muy grande."
        Just n  -> return (TkNum n, pos)

-- | Tokenizes the True value.
trueTokenizer :: Parser TokenPos
trueTokenizer = do
    pos <- getPosition 
    string "true"
    notFollowedBy alphaNum 
    spaces
    return (TkTrue,pos)

-- | Tokenizes the False value.
falseTokenizer :: Parser TokenPos
falseTokenizer = reservedTokenizerGen "false" TkFalse

-- | Tokenizes the mod value.
modTokenizer :: Parser TokenPos
modTokenizer = reservedTokenizerGen "mod" TkMod

-- | Tokenizes the while value.
whileTokenizer :: Parser TokenPos
whileTokenizer = reservedTokenizerGen "while" TkWhile

-- | Tokenizes the if value.
ifTokenizer :: Parser TokenPos
ifTokenizer = reservedTokenizerGen "if" TkIf

-- | Generates a parser for reserved words
reservedTokenizerGen :: String -> Token -> Parser TokenPos
reservedTokenizerGen r tk
    =  string r 
    *> notFollowedBy alphaNum 
    *> spaces
    *> ((tk,) <$> getPosition )

-- | Tokenizes a reserved workd
reservedTokenizer :: Parser TokenPos
reservedTokenizer = choice 
    [ trueTokenizer
    , falseTokenizer
    , modTokenizer
    , whileTokenizer
    , ifTokenizer
    ]


-- | Tokenizes the True value.
trueTokenizer' :: Parser TokenPos
trueTokenizer' = string ("true" :: [Char]) *> spaces *> ((TkTrue, ) <$> getPosition )

-- | Tokenizes a single string
singleToken :: Parser TokenPos
singleToken = foldl1 (<|>)  
    [ numTokenizer
    , try reservedTokenizer
    , idTokenizer
    ]

-- | Tokenizes the entirety of the input 
manyToken :: Parser [TokenPos]
manyToken = do
    spaces
    tokens   <- many singleToken
    pos      <- getPosition 
    i        <- getInput 
    eof <?>  ("ERROR: lexer(" ++ show i ++ ") ==> token invalido en la entrada: " ++ [last i])
    return tokens


{-
-- | a
toLexerError :: ParseError -> [(String, String)] 
toLexerError error' = trace (show eTrace) $ zip (repeat errorPos') errorMsgs
    where
        eTrace     = messageString <$> errorMessages error'
        errorMsgs  = messageString <$> errorMessages error'
        errorPos'  = show $ errorPos  error'



-- | a
lexer :: String ->  Either [(String, String)] [TokenPos]
lexer =  first toLexerError . parse manyToken "" 
-}

showTokenPos :: String -> [TokenPos] -> String
showTokenPos input tokens = "OK: lexer(" ++  show input ++  ") ==> " ++ show (map fst tokens)

