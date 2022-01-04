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
    | TkString       -- ^ @ string @
    | TkFloat        -- ^ @ float @
    | TkDQuote       -- ^ @ " @
    deriving (Eq,Show)

newtype PrettyToken = PT Token 


instance Show PrettyToken where
    show (PT (TkId var))     = var    
    show (PT (TkNum n))      = show n
    show (PT TkTrue)         = "True"   
    show (PT TkFalse)        = "False"   
    show (PT TkOpenPar)      = "("   
    show (PT TkClosePar)     = ")"   
    show (PT TkPower)        = "^"   
    show (PT TkPlus)         = "+"   
    show (PT TkMinus)        = "-"   
    show (PT TkNot)          = "!"   
    show (PT TkMult)         = "*"   
    show (PT TkMod)          = "Mod"   
    show (PT TkLT)           = "<"   
    show (PT TkLE)           = "<="   
    show (PT TkGE)           = ">="   
    show (PT TkGT)           = ">"   
    show (PT TkEQ)           = "="   
    show (PT TkNE)           = "<>"   
    show (PT TkAnd)          = "&&"   
    show (PT TkOr)           = "||"   
    show (PT TkQuote)        = "'"   
    show (PT TkComma)        = ","   
    show (PT TkAssign)       = ":="   
    show (PT TkSemicolon)    = ";"  
    show (PT TkYields)       = "=>"  
    show (PT TkRArrow)       = "->"  
    show (PT TkLArrow)       = "<-"  
    show (PT TkOpenBracket)  = "["
    show (PT TkCloseBracket) = "]"
    show (PT TkOpenBrace)    = "{"
    show (PT TkCloseBrace)   = "}"
    show (PT TkDot)          = "."
    show (PT TkColon)        = ":"
    show (PT TkColonColon)   = "::"
    show (PT TkWhile)        = "while"
    show (PT TkIf)           = "if"
    show (PT TkInt)          = "int"
    show (PT TkBool)         = "bool"
    show (PT TkType)         = "type"
    show (PT TkLazy)         = "lazy"
    show (PT TkString)       = "string"
    show (PT TkFloat)        = "float"


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
op = ["(",")","^","+","-","!","*","%","<","<=",">=",">","=","&&","||","<>"]
tkOp = [ TkOpenPar, TkClosePar, TkPower, TkPlus, TkMinus, TkNot, TkMult, TkMod, TkLT, TkLE, TkGE, TkGT, TkEQ, TkAnd, TkOr, TkNE]

-- | Symbols of LIPS-USB language
sim :: [[Char]]
sim = ["'",",",":=",";","=>","->","<-","[","]","{","}",".",":","::"]
tkSim = [ TkQuote, TkComma, TkAssign, TkSemicolon, TkYields, TkRArrow, TkLArrow, TkOpenBracket, TkCloseBracket, TkOpenBrace, TkCloseBrace, TkDot, TkColon, TkColonColon ]

-- | Reserverd words of LIPS-USB language
reservedWord :: [[Char]]
reservedWord = [ "int", "bool", "type", "false", "true", "lazy", "while","if", "string", "float" ]
tkReservedWord = [TkInt, TkBool, TkType, TkFalse, TkTrue, TkLazy, TkWhile, TkIf, TkString, TkFloat]

-- | Special characters = Opertaros U Symbols
spe :: [[Char]]
spe = op ++ sim

-- | Characters to ignore
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

-- | Boolean functions to know if a string hace an alphabetic character
haveAlpha :: String -> Bool
haveAlpha [xs] = xs `elem` ['A'..'Z'] ++ ['a'..'z'] ++ "_"
haveAlpha (xs:x)
    | xs `elem` ['A'..'Z'] ++ ['a'..'z'] ++ "_" = True 
    | otherwise = haveAlpha x
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
    | all isDigit x         = if checkOverflow x 
        then Right $ TkNum $ read x 
        else Left ("ERROR: lexer(" ++ show x ++ ") ==> Integer overflow/underflow",col)
    -- It's variable identifier
    | isId x                = Right $ TkId x
    | not (haveAlpha x)
        = Left ("ERROR: lexer(" ++ show x ++ ") ==> Operador no reconocido en LIPS",col)
    | isDigit (head x)
        = Left ("ERROR: lexer(" ++ show x ++ ") ==> Las variables no pueden comenzar con numeros",col)
    | otherwise
        = Left ("ERROR: lexer(" ++ show x ++ ") ==> Las variables solo pueden contener caracteres alfanumericos o underscore",col)

-- | Checks for over/underflow.
checkOverflow :: String -> Bool
checkOverflow xs = case xs of
    '-':_ -> (lxs <= lminb) && (signum ( read xs :: Int ) == -1)
    _     -> (lxs <= lmaxb) && (signum ( read xs :: Int ) >= 0)
    where
        lminb = length $ show (minBound :: Int)
        lmaxb = length $ show (maxBound :: Int)
        lxs   = length xs


-- | Convert a String to ( list Tokens or Error )
manyToken 
    :: String                         -- ^ Inicial String 
    -> Either [(String,Int)] [Token]  -- ^ (Error,position) or list tokens
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



