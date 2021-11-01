{- |
Module      : Lexer
Description : Provides necessary tools for lexing.
Maintainer  : 15-11139@usb.ve, 16-10400@usb.ve 17-10538@usb.ve
Stability   : experimental
Portability : POSIX
-}
module Lexer.Lexer where

import Data.Char ( isDigit )
import Data.Bifunctor ( Bifunctor(first) )

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
    | TkInt          -- ^ @ int @
    | TkBool         -- ^ @ bool @
    | TkType         -- ^ @ type @
    | TkLazy         -- ^ @ lazy @
    deriving (Show,Eq)

-- | Auxiliary type, will provide a "logged" version of Either
newtype LEither a = L (Either [(String,Int)] a)


-- | Unwraps the aux type.
unwrapL :: LEither a -> Either [(String,Int)] a
unwrapL (L a) = a 

-- The functor instance will be the same
instance Functor LEither where
    fmap f (L xs) = L $ fmap f xs

-- The applicative instance is mostly the same
instance Applicative LEither where
    pure = L . Right
    -- but it concatenates the errors.
    (L (Left logs)) <*> (L (Left err)) = L $ Left (err ++ logs)
    (L f) <*> (L a)                    = L $ f <*> a

-- ========= Symbols and operators =========

-- | Operators of LIPS-USB language
op :: [[Char]]
op = ["(",")","^","+","-","!","*","%","<","<=",">=",">","=","&&","||"]
tkOp = [ TkOpenPar, TkClosePar, TkPower, TkPlus, TkMinus, TkNot, TkMult, TkMod, TkLT, TkLE, TkGE, TkGT, TkEQ, TkAnd, TkOr ]

-- | Symbols of LIPS-USB language
sim :: [[Char]]
sim = ["‘",",",":=",";","=>","->","<-","[","]","{","}",".",":","::"]
tkSim = [ TkQuote, TkComma, TkAssign, TkSemicolon, TkYields, TkRArrow, TkLArrow, TkOpenBracket, TkCloseBracket, TkOpenBrace, TkCloseBrace, TkDot, TkColon, TkColonColon ]

-- | Reserverd words of LIPS-USB language
reservedWord :: [[Char]]
reservedWord = [ "int", "bool", "type", "false", "true", "lazy", "while","if" ]
tkReservedWord = [TkInt, TkBool, TkType, TkFalse, TkTrue, TkLazy, TkWhile, TkIf]

-- | Special characters = Opertaros U Symbols
spe :: [[Char]]
spe = op ++ sim

-- | Characters to ignores
igChar :: [Char]
igChar = [ '\n', ' ', '\t', '\r' ]

-- ========= Functions of the Lexer =========

-- | Bolean function to know if a string is a possible TkId
isId 
    :: String   -- ^ Input String
    -> Bool     -- ^ True if the string can be a Tkid or otherwise False
isId [] = False 
isId (fc:r) = (fc `elem` firstChar) && all (`elem` rest) r
    where
        firstChar = ['A'..'Z'] ++ ['a'..'z'] ++ "_"
        rest      = firstChar ++ ['0'..'9']

{- 
Function that take a string and converts it to a list of string 
where each element of the list belong to spe (special symbol) or it's
a potential number or variable. The function should be called with:
split [] originalString
-}
split 
    :: [Char]   -- ^ String rest, Alphanumeric strings between special characters
    ->[Char]    -- ^ String to split
    -> [[Char]] -- ^ list of Strings, where each one is a potential token
split rest [] = if null rest then [] else [rest] 
split rest [x]
    | (x `elem` igChar) && (null rest) = []
    | (x `elem` igChar) && ([x] `elem` spe) = [rest]
    | (x `elem` igChar) = [ rest ]
    | null rest = [[x]]
    | [x] `elem` spe = [rest,[x]]
    | otherwise = [ rest++[x] ]
split rest (x1:x2:xs) 
    -- If character i-1 is a space, tab or end of line, skip it
    | x1 `elem` igChar = z ++ split [] (x2:xs)
    -- If chacacter i-1 conctat with i is a special string
    | ([x1,x2]) `elem` spe = z ++ [[x1,x2]] ++ split [] xs
    -- If only the first character is a special string
    | [x1] `elem` spe = z ++ [[x1]] ++ split [] (x2:xs)
    -- The first character belongs to a number or a variable
    | otherwise = split (rest++[x1]) (x2:xs)
    where z = if null rest then [] else [rest] 

-- | Convert a list of Strings to a list of pairs, (sameString,column)
cols 
    :: [String]         -- list of Strings
    -> [(String,Int)]   -- List of (Strings,sumLenPrefix)
cols []     = []
cols (x:xs) = (x,0) : map (fmap (+length x)) (cols xs)

-- | Find the position of an element in a list
findPos list elt = [index | (index, e) <- zip [0..] list, e == elt]

-- | Convert a pair (String,column) to a (Token or Error) element
tokenizer 
    :: ([Char],Int)                 -- ^ Pair (String,column)
    -> Either (String,Int) Token    -- ^ Error or token
tokenizer (x,col)
    -- Operator
    | x `elem` op           = Right $ tkOp !! head (findPos op x)
    -- Symbol
    | x `elem` sim          = Right $ tkSim !! head (findPos sim x)
    -- Reserved Word
    | (x `elem` reservedWord)
        && isId x           = Right $ tkReservedWord !! head (findPos reservedWord x)
    -- It's a number
    | all isDigit x         = Right $ TkNum $ read x
    -- It's variable identifier
    | isId x                = Right $ TkId x
    | otherwise             = Left ("ERROR: lexer(" ++ show x ++ ") ==> Inicializador de identificador invalido",col)

-- | Convert a String to ( Token or Error )
manyToken 
    :: String                         -- ^ Inicial String 
    -> Either [(String,Int)] [Token]  -- ^ (Error,position) or token
manyToken xs = unwrapL $ traverse ( f . tokenizer) $ cols ( split [] xs )
    where
        f (Left x)  = L $ Left [x]
        f (Right a) = L $ Right a

-- | Lexes a string.
lexer :: String -> String
lexer x = parsed
    where
        f (Left x)  = L $ Left [x]
        f (Right a) = L $ Right a

        traversed = unwrapL $ fmap (showTokenPos x) $ traverse ( f . tokenizer) $ cols ( split [] x )
        parsed = either unlines id $ first (fmap fst) traversed

-- | Aux function that transforms the string into an ok message.
showTokenPos :: String -> [Token] -> String
showTokenPos input tokens = "OK: lexer(" ++  show input ++  ") ==> " ++ show tokens



