module Lexer.Lexer where

import Data.Char

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

-- ========= Symbols and operators =========

-- | Operators of LIPS-USB language
op :: [[Char]]
op = ["(",")","^","+","-","!","*","%","<","<=",">=",">","=","&&","||"]
tkOp = [ TkOpenPar, TkClosePar, TkPower, TkPlus, TkMinus, TkNot, TkMult, TkMod, TkLT, TkLE, TkGE, TkGT, TkEQ, TkAnd, TkOr ]

-- | Symbols of LIPS-USB language
sim :: [[Char]]
sim = ["‘",",",":=",";","=>","->","<-","[","]","{","}",".",":","::","while","if"]
tkSim = [ TkQuote, TkComma, TkAssign, TkSemicolon, TkYields, TkRArrow, TkLArrow, TkOpenBracket, TkCloseBracket, TkOpenBrace, TkCloseBrace, TkDot, TkColon, TkColonColon, TkWhile, TkIf ]

-- | Reserverd words of LIPS-USB language
reservedWord :: [[Char]]
reservedWord = [ "int", "bool", "type", "false", "true", "lazy" ]
tkReservedWord = [TkInt, TkBool, TkType, TkFalse, TkTrue, TkLazy ]

-- | Special characters = Opertaros U Symbols U Reserved Words
spe :: [[Char]]
spe = op ++ sim ++ reservedWord

-- | Characters to ignores
igChar :: [Char]
igChar = [ '\n', ' ', '\t' ]

-- ========= Functions of the Lexer =========

-- | Bolean function to know if a string is a possible TkId
isId 
    :: String   -- Input String
    -> Bool     -- True if the string can be a Tkid or otherwise False
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
    :: ([Char],Int)                 -- Pair (String,column)
    -> Either (String,Int) Token    -- Error or token
tokenizer (x,col)
    -- Operator
    | x `elem` op           = Right $ tkOp !! head (findPos op x)
    -- Symbol
    | x `elem` sim          = Right $ tkSim !! head (findPos sim x)
    -- Reserved Word
    | x `elem` reservedWord = Right $ tkReservedWord !! head (findPos reservedWord x)
    -- It's a number
    | all isDigit x         = Right $ TkNum $ read x
    -- It's variable identifier
    | isId x                = Right $ TkId x
    | otherwise             = Left ("ERROR: lexer(" ++ show x ++ ") ==> Inicializador de identificador invalido",col)

-- | Convert a String to ( Token or Error )
manyToken 
    :: String                       -- Inicial String 
    -> Either (String,Int) [Token]  -- (Error,position) or token
manyToken xs = traverse tokenizer $ cols ( split [] xs )

lexer :: String -> String
lexer x = either fst unwords $ traverse (fmap show . tokenizer) $ cols ( split [] x )

showTokenPos :: String -> [Token] -> String
showTokenPos input tokens = "OK: lexer(" ++  show input ++  ") ==> " ++ show tokens