import Data.Char

-- ////////////// Simbolos y operadores ////////////////
op :: [[Char]]
op = ["(",")","^","+","-","!","*","%","<","<=",">=",">","=","&&","||"]
tkOp = [ "TkOpenPar", "TkClosePar", "TkPower", "TkPlus", "TkMinus", "TkNot", "TkMult", "TkMod", "TkLT", "TkLE", "TkGE", "TkGT", "TkEQ", "TkAnd", "TkOr" ]

sim :: [[Char]]
sim = ["‘",",",":=",";","=>","->","<-","[","]","{","}",".",":","::","while","if"]
tkSim = [ "TkQuote", "TkComma", "TkAssign", "TkSemicolon", "TkYields", "TkRArrow", "TkLArrow", "TkOpenBracket", "TkCloseBracket", "TkOpenBrace", "TkCloseBrace", "TkDot", "TkColon", "TkColonColon", "TkWhile", "TkIf" ]

reservedWord :: [[Char]]
reservedWord = [ "int", "bool", "type", "false", "true" ]
tkReservedWord = ["TkInt", "TkBool", "TkType", "TkFalse", "TkTrue" ]

spe :: [[Char]]
spe = op ++ sim ++ reservedWord

--      Resto   StrOriginal  listaStr
split :: [Char] ->[Char] -> [[Char]]
split rest [] = if null rest then [] else [rest] 
split rest [x]
    | rest == [] = [[x]]
    | [x] `elem` spe = [rest,[x]]
    | otherwise = [ rest++[x] ]
split rest (x1:x2:xs) 
    | x1 == ' ' = z ++ split [] (x2:xs)
    | ([x1]++[x2]) `elem` spe = z ++ [[x1,x2]] ++ split [] xs
    | [x1] `elem` spe = z ++ [[x1]] ++ split [] (x2:xs)
    | otherwise = split (rest++[x1]) (x2:xs)
    where z = if null rest then [] else [rest] 


findPos list elt = [index | (index, e) <- zip [0..] list, e == elt]

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

lexer x = map tokenizer ( split [] x )