{- |
Module      : Parser
Description : Provides necessary tools for lexing.
Maintainer  : 15-11139@usb.ve, 16-10400@usb.ve 17-10538@usb.ve
Stability   : experimental
Portability : POSIX
-}

-- =========== Split a list of tokens ===========
module Parser.Parser where

import AST.AST
import Data.Maybe
import Parser.Tokens


-- Auxiliar Function to know the deep of the parenthesis, left assoc
valParL :: Token -> Int
valParL x
    | x == TkOpenPar = -1
    | x == TkClosePar = 1
    | otherwise = 0

-- Auxiliar Function to know the deep of the parenthesis, right assoc
valParR :: Token -> Int
valParR x = - valParL x

{--
Function To know if a token is a terminal Token, depends on the
associativity for that it use ( valParL or valParR )
--}
isEndToken :: Token -> (Token->Int) -> Bool
isEndToken tk fPar = (isTkNum tk) || (isTkId tk) || ( 1 == (fPar tk))

-- flagToLeft lookingSymbols listOfTokens
splitListTokens :: Int -> [Token] -> [Token] -> Maybe ( [Token], Token, [Token] )
splitListTokens flagL s lista
    -- Left associativity
    | flagL == 1 = splitListTokensAux s lista [] 0 valParL
    -- Right associativity
    | otherwise = do 
        (\(xs, z, ys) -> (reverse ys, z, reverse xs)) 
        <$> splitListTokensAux s (reverse lista) [] 0 valParR

--- splitListTokens ( Buscado, lista, [], 0, funcionParentesis )
splitListTokensAux :: [Token] -> [Token] -> [Token] -> Int -> (Token -> Int) -> Maybe ( [Token], Token, [Token] )
splitListTokensAux s [] end cnt fCnt = Nothing
splitListTokensAux s [a] end cnt fCnt = Nothing
splitListTokensAux listaS sta end cnt fCnt 
    | cnt < 0 = Nothing
    | (l `elem` listaS) && (cnt==0) && validS = Just ( start, l, end )
    | otherwise = splitListTokensAux listaS start ([l]++end) ( cnt + fCnt(l) ) fCnt
    where 
        l = last sta
        start = init sta
        validS = isEndToken (last start) fCnt

-- =============== Grammar ==============

firstLast::[a]->[a]
firstLast [] = []
firstLast [x] = []
firstLast xs = tail (init xs)

logicOr :: [Token] -> Maybe Expr
logicOr lista
    | isNothing z = logicAnd lista 
    | (isNothing z1) || (isNothing z2) = Nothing
    | tk == TkOr = Just $ Or ex1 ex2
    where 
        z = splitListTokens 1 [TkOr] lista
        (listL,tk,listR) = fromJust z 
        z1 = logicOr listL 
        z2 = logicAnd listR
        ex1 = fromJust z1
        ex2 = fromJust z2

logicAnd :: [Token] -> Maybe Expr
logicAnd lista
    | isNothing z = equality lista 
    | (isNothing z1) || (isNothing z2) = Nothing
    | tk == TkAnd = Just $ And ex1 ex2
    where 
        z = splitListTokens 1 [TkAnd] lista
        (listL,tk,listR) = fromJust z 
        z1 = logicAnd listL 
        z2 = equality listR
        ex1 = fromJust z1
        ex2 = fromJust z2

equality :: [Token] -> Maybe Expr
equality lista
    | isNothing z = comparison lista 
    | (isNothing z1) || (isNothing z2) = Nothing
    | tk == TkEQ = Just $ AST.AST.EQ ex1 ex2
    | tk == TkNE = Just $ NEQ ex1 ex2
    where 
        z = splitListTokens 1 [TkEQ, TkNE] lista
        (listL,tk,listR) = fromJust z 
        z1 = equality listL 
        z2 = comparison listR
        ex1 = fromJust z1
        ex2 = fromJust z2

comparison :: [Token] -> Maybe Expr
comparison lista
    | isNothing z = expression lista 
    | (isNothing z1) || (isNothing z2) = Nothing
    | tk == TkLT = Just $ AST.AST.LT ex1 ex2
    | tk == TkLE = Just $ LE ex1 ex2
    | tk == TkGT = Just $ AST.AST.GT ex1 ex2
    | tk == TkGE = Just $ GE ex1 ex2
    where 
        z = splitListTokens 1 [TkLT, TkLE, TkGT, TkGE] lista
        (listL,tk,listR) = fromJust z 
        z1 = comparison listL 
        z2 = expression listR
        ex1 = fromJust z1
        ex2 = fromJust z2

expression :: [Token] -> Maybe Expr
expression lista
    | isNothing z = term lista 
    | (isNothing z1) || (isNothing z2) = Nothing
    | tk == TkPlus = Just $ Plus ex1 ex2
    | tk == TkMinus = Just $ Minus ex1 ex2
    where 
        z = splitListTokens 1 [TkPlus, TkMinus] lista
        (listL,tk,listR) = fromJust z 
        z1 = expression listL 
        z2 = term listR
        ex1 = fromJust z1
        ex2 = fromJust z2

term :: [Token] -> Maybe Expr
term lista
    | isNothing z = unaryExpression lista 
    | (isNothing z1) || (isNothing z2) = Nothing
    | tk == TkMult = Just $ Times ex1 ex2
    | tk == TkMod = Just $ Mod ex1 ex2
    where 
        z = splitListTokens 1 [TkMult, TkMod] lista
        (listL,tk,listR) = fromJust z 
        z1 = term listL 
        z2 = unaryExpression listR
        ex1 = fromJust z1
        ex2 = fromJust z2


unaryExpression :: [Token] -> Maybe Expr
unaryExpression [] = powExpression []
unaryExpression (z:lista) 
    | z == TkNot = do
        temp <- unaryExpression lista
        Just $ Negate temp
    | z == TkPlus = do
        temp <- unaryExpression lista
        Just $ Pos temp
    | z == TkMinus = do 
        temp <- unaryExpression lista
        Just $ Pos temp -- CAMBIAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAR
    | otherwise = powExpression ([z]++lista)

powExpression :: [Token] -> Maybe Expr
powExpression lista
    | isNothing z = factor lista 
    | (isNothing z1) || (isNothing z2) = Nothing
    | tk == TkPower = Just $ Pow ex1 ex2
    where 
        -- Right associativity
        z = splitListTokens 0 [TkPower] lista
        (listL,tk,listR) = fromJust z 
        z1 = factor listL 
        z2 = powExpression listR
        ex1 = fromJust z1
        ex2 = fromJust z2

factor :: [Token] -> Maybe Expr 
factor [] = Nothing
factor lista 
    | ((head lista) == TkOpenPar) && ((last lista) == TkClosePar) = logicOr $ firstLast lista
    | (length lista) /= 1 = Nothing
    | isTkNum z = Just $ C $ NumConstant $ getTkNum z
    | isTkId z = Just $ Var $ getTkId z
    | otherwise = Nothing 
    where z = head lista

-- =============== Test cases ==============

--lista4 = [ TkNum 3, TkPlus, TkOpenPar, TkNum 2, TkPlus, TkNum 13, TkClosePar ]
--lista4 = [ TkOpenPar, TkNum 2, TkPlus, TkNum 13, TkClosePar, TkPlus, TkId "var" ]
--lista4 = [ TkOpenPar, TkOpenPar, TkNum 2, TkPlus, TkNum 13, TkClosePar, TkClosePar, TkPlus, TkNum 24 ]

--lista4 = [ TkOpenPar, TkOpenPar, TkNum 2, TkPlus, TkNum 13, TkClosePar, TkClosePar, TkPlus, TkNum 24 ]
--lista4 = [ TkNum 1, TkPower, TkNum 2, TkPower, TkNum 3 ]
--lista4 = [ TkNum 1, TkPlus, TkNum 2, TkPlus, TkOpenPar, TkNum 3, TkPower, TkNum 5, TkClosePar, TkPower, TkNum 4 ]

--lista4 = [ TkNum 1, TkPlus, TkMinus, TkPlus, TkNum 4 ]

--lista4 = [ TkNum 1, TkPlus, TkNum 2, TkMult, TkNum 3, TkMult, TkOpenPar, TkNum 4, TkPlus, TkNum 5, TkClosePar, TkMult, TkNum 6 ]

--lista4 = [TkNum 1, TkPlus, TkNum 2, TkPlus, TkNum 3]

--lista4 = [TkNum 1, TkPlus, TkNum 6, TkGE, TkNum 5]
--lista5 = [TkNum 1, TkPlus, TkNum 2, TkMult, TkNum 3, TkMult, TkOpenPar, TkNum 4, TkPlus, TkNum 5, TkClosePar, TkMult, TkNum 6,  TkGE, TkNum 5]
--lista6 = [TkNum 1, TkPlus, TkNum 2, TkMult, TkNum 3, TkMult, TkOpenPar, TkNum 4, TkPlus, TkNum 5, TkClosePar, TkMult, TkNum 6,  TkLE, TkNum 5, TkMult, TkNum 1, TkPlus, TkNum 2, TkMult, TkNum 3, TkMult, TkOpenPar, TkNum 4, TkPlus, TkNum 5, TkClosePar, TkMult, TkNum 6]

--lista6 = [TkNum 1, TkPlus, TkNum 2, TkMult, TkNum 3, TkMult, TkOpenPar, TkNum 4, TkPlus, TkNum 5, TkClosePar, TkMult, TkNum 6,  TkNE, TkNum 5, TkMult, TkNum 1, TkPlus, TkNum 2, TkMult, TkNum 3, TkMult, TkOpenPar, TkNum 4, TkPlus, TkNum 5, TkClosePar, TkMult, TkNum 6]
--lista7 = [TkNum 1, TkPlus, TkNum 2, TkMult, TkNum 3, TkMult, TkOpenPar, TkNum 4, TkPlus, TkNum 5, TkClosePar, TkMult, TkNum 6,  TkNE, TkNum 5, TkMult, TkNum 1, TkPlus, TkNum 2, TkMult, TkNum 3, TkMult, TkOpenPar, TkNum 4, TkPlus, TkNum 5, TkClosePar, TkMult, TkNum 6, TkAnd, TkNum 5, TkGT, TkNum 1]

lista7 = [TkNum 1, TkPlus, TkNum 2, TkMult, TkNum 3, TkMult, TkOpenPar, TkNum 4, TkPlus, TkNum 5, TkClosePar, TkMult, TkNum 6,  TkNE, TkNum 5, TkMult, TkNum 1, TkPlus, TkNum 2, TkMult, TkNum 3, TkMult, TkOpenPar, TkNum 4, TkPlus, TkNum 5, TkClosePar, TkMult, TkNum 6, TkOr, TkNum 5, TkGT, TkNum 1]
lista8 = [TkNum 1, TkPlus, TkNum 2, TkMult, TkNum 3, TkMult, TkOpenPar, TkNum 4, TkPlus, TkNum 5, TkClosePar, TkMult, TkNum 6,  TkNE, TkNum 5, TkMult, TkNum 1, TkPlus, TkNum 2, TkMult, TkNum 3, TkMult, TkOpenPar, TkNum 4, TkPlus, TkNum 5, TkClosePar, TkMult, TkNum 6, TkOr, TkNum 5, TkGT, TkNum 1, TkAnd, TkId "a", TkEQ, TkId "b"]
lista9 = [TkOpenPar, TkId "a", TkGE, TkId "b", TkOr, TkId "c", TkEQ, TkId "b", TkClosePar, TkAnd, TkOpenPar, TkId "d", TkLT, TkId "b", TkOr, TkId "a", TkNE, TkId "c", TkClosePar]

--prettyPrintS $ E $ fromJust $ expression lista4

-- Quelle galère
-- ''  'E'+'E'+''E''  '' 
