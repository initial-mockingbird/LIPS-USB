{- |
Module      : Parser
Description : Provides necessary tools for lexing.
Maintainer  : 15-11139@usb.ve, 16-10400@usb.ve 17-10538@usb.ve
Stability   : experimental
Portability : POSIX
-}

module Parser.Parser where

import AST.AST
import Data.Maybe

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

f x = 2*x

g ::  Int -> Maybe Int
g x
    | x == 1 = (Just 1)
    | x == 2 = (Nothing)
    | otherwise = do
        gval <- g(x-2)
        return $ gval + 2

-- ======= Know if a list of token have correct parenthesis =======

validParen :: [Token] -> Bool
validParen li = validParenAux li 0

validParenAux :: [Token] -> Int -> Bool
validParenAux [] cnt = if cnt==0 then True else False
validParenAux (x:xs) cnt
    | cnt < 0 = False
    | x==TkOpenPar = validParenAux xs (cnt + 1)
    | x==TkClosePar = validParenAux xs (cnt - 1) 
    | otherwise = validParenAux xs cnt 

lista :: [Token]
lista = [ TkOpenPar, TkOpenPar, TkClosePar, TkOpenPar, TkClosePar, TkClosePar ]

lista2 = [ TkOpenPar, TkPlus, TkClosePar ]
temp = ( init lista2, last lista2 )

-- =========== Split a list of tokens ===========

splitListTokens :: [Token] -> [Token] -> Maybe ( [Token], Token, [Token] )
splitListTokens s lista = splitListTokensAux s lista [] 0

--- splitListTokens ( Buscado, lista, [], 0 )
splitListTokensAux :: [Token] -> [Token] -> [Token] -> Int -> Maybe ( [Token], Token, [Token] )
splitListTokensAux s [] end cnt = Nothing
splitListTokensAux lista sta end cnt
    | cnt < 0 = Nothing
    | ((l `elem` lista) && (cnt==0)) = Just ( start, l, end )
    | l == TkOpenPar = splitListTokensAux lista start ([l]++end) (cnt-1)
    | l == TkClosePar = splitListTokensAux lista start ([l]++end) (cnt+1)
    | otherwise = splitListTokensAux lista start ([l]++end) cnt
    where 
        l = last sta
        start = init sta

lista3 = [ TkOpenPar, TkPlus, TkClosePar, TkPlus, TkOpenPar, TkPlus, TkClosePar ]
spe = [ TkPlus ]
z = splitListTokens spe lista4

-- =============== Grammar ==============

isTkId :: Token -> Bool
isTkId (TkId _) = True
isTkId _ = False

isTkNum :: Token -> Bool
isTkNum (TkNum _) = True
isTkNum _ = False

getTkNum :: Token -> Int 
getTkNum ( TkNum x ) = x

getTkId :: Token -> String
getTkId ( TkId x ) = x

firstLast::[a]->[a]
firstLast [] = []
firstLast [x] = []
firstLast xs = tail (init xs)

v1 :: Expr
v1 = Var "exito"

expNull :: Expr
expNull = Var "Error"

expression :: [Token] -> Maybe Expr
expression lista
    | isNothing z = factor lista 
    | (isNothing z1) || (isNothing z2) = Nothing
    | tk == TkPlus = Just $ Plus ex1 ex2
    | tk == TkMinus = Just $ Minus ex1 ex2
    | otherwise = Nothing
    where 
        z = splitListTokens [TkPlus, TkMinus] lista
        (listL,tk,listR) = fromJust z 
        z1 = expression listL
        z2 = factor listR
        ex1 = fromJust z1
        ex2 = fromJust z2

factor :: [Token] -> Maybe Expr 
factor lista 
    | (length lista) == 0 = Nothing
    | ((head lista) == TkOpenPar) && ((last lista) == TkClosePar) = expression $ firstLast lista
    | (length lista) /= 1 = Nothing
    | isTkNum z = Just $ C $ NumConstant $ getTkNum z
    | isTkId z = Just $ Var $ getTkId z
    | otherwise = Nothing 
    where z = head lista

lista4 = [ TkId "var1", TkPlus, TkOpenPar, TkNum 2, TkPlus, TkNum 13, TkClosePar ]
--prettyPrintS $ E $ fromJust $ factor lista4

