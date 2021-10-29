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
    deriving (Show,Eq)

-- ////////////// Simbolos y operadores ////////////////
op :: [[Char]]
op = ["(",")","^","+","-","!","*","%","<","<=",">=",">","=","&&","||"]
tkOp = [ TkOpenPar, TkClosePar, TkPower, TkPlus, TkMinus, TkNot, TkMult, TkMod, TkLT, TkLE, TkGE, TkGT, TkEQ, TkAnd, TkOr ]

sim :: [[Char]]
sim = ["‘",",",":=",";","=>","->","<-","[","]","{","}",".",":","::","while","if"]
tkSim = [ TkQuote, TkComma, TkAssign, TkSemicolon, TkYields, TkRArrow, TkLArrow, TkOpenBracket, TkCloseBracket, TkOpenBrace, TkCloseBrace, TkDot, TkColon, TkColonColon, TkWhile, TkIf ]

reservedWord :: [[Char]]
reservedWord = [ "int", "bool", "type", "false", "true" ]
tkReservedWord = [TkInt, TkBool, TkType, TkFalse, TkTrue ]

spe :: [[Char]]
spe = op ++ sim ++ reservedWord

--      Resto   StrOriginal  listaStr
split :: [Char] ->[Char] -> [[Char]]
split rest [] = if null rest then [] else [rest] 
split rest [x]
    | null rest = [[x]]
    | [x] `elem` spe = [rest,[x]]
    | otherwise = [ rest++[x] ]
split rest (x1:x2:xs) 
    | x1 == ' ' = z ++ split [] (x2:xs)
    | ([x1]++[x2]) `elem` spe = z ++ [[x1,x2]] ++ split [] xs
    | [x1] `elem` spe = z ++ [[x1]] ++ split [] (x2:xs)
    | otherwise = split (rest++[x1]) (x2:xs)
    where z = if null rest then [] else [rest] 


findPos list elt = [index | (index, e) <- zip [0..] list, e == elt]

{-
tokenizer :: [Char] -> [Char]
tokenizer x
    -- Operator
    | x `elem` op = tkOp !! ((findPos op x)!!0)
    -- Symbol
    | x `elem` sim = tkSim !! ((findPos sim x)!!0)
    -- Reserved Word
    | x `elem` reservedWord = tkReservedWord !! ((findPos reservedWord x)!!0)
    -- It's a number
    | all isDigit x = "TkNum("++x++")"
    -- It's variable identifier
    | all isAlphaNum x = "TkId('"++ x ++"')"
    | otherwise = "Error: "++x ++"==> interpretación no implementada"
-}

tokenizer :: ([Char],Int) -> Either (String,Int) Token
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


manyToken :: String -> Either (String,Int) [Token]
manyToken xs = traverse tokenizer $ cols ( split [] xs )

cols :: [String] -> [(String,Int)]
cols []     = []
cols (x:xs) = (x,0) : map (fmap (+length x)) (cols xs)

isId :: String -> Bool
isId [] = False 
isId (fc:r) = (fc `elem` firstChar) && all (`elem` rest) r
    where
        firstChar = ['A'..'Z'] ++ ['a'..'z'] ++ "_"
        rest      = firstChar ++ ['0'..'9']

lexer :: String -> String
lexer x = either fst unwords $ traverse (fmap show . tokenizer) $ cols ( split [] x )

showTokenPos :: String -> [Token] -> String
showTokenPos input tokens = "OK: lexer(" ++  show input ++  ") ==> " ++ show tokens