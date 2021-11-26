module PParser.PParser where



import Lexer.Lexer hiding (isId)
import Text.Parsec
import Text.Parsec.Combinator

import Data.Functor.Identity
import AST.AST
import qualified Control.Applicative as A (optional)
import Prelude hiding (GT,LT,GE,LE)


type SP m a = ParsecT [Token] m Identity a

isId :: Monad m => ParsecT [Token] u m Expr
isId = tokenPrim  show g f
    where
        f (TkId x) = Just $ Var x
        f _          = Nothing
        g pos _ _    = incSourceColumn pos 1

isNum :: Monad m => ParsecT [Token] u m Expr
isNum = tokenPrim show g f
    where
        f (TkNum n) = Just $ C $ NumConstant n
        f _           = Nothing
        g pos _  _    = incSourceColumn pos 1

isBool :: Monad m => ParsecT [Token] u m Expr
isBool = tokenPrim show g f
    where
        f TkTrue  = Just $ C $ BConstant True
        f TkFalse = Just $ C $ BConstant False 
        f _           = Nothing
        g pos _  _    = incSourceColumn pos 1 

isFapp :: SP m Expr
isFapp = do
    (Var fName) <- isId
    pOP
    fArgs <- pArgs
    pCP
    return $ FApp fName fArgs


pArgs :: SP m [Expr]
pArgs = pNEList <|> (eof >> pure [])

pNEList :: SP m [Expr]
pNEList = pure <$> pP0 <|> (pNEList >>= \args -> pComma >> fmap (:args) pP0)


genSymbolParser :: Monad m => Token -> ParsecT [Token] u m Token
genSymbolParser tk = tokenPrim show g f
    where
        f tk'
            | tk' == tk = Just tk
            | otherwise = Nothing
        g pos _  _    = incSourceColumn pos 1


pS, pM, pNot, pMult, pOP, pCP, pQuote                :: Monad m => ParsecT [Token] u m Token
pMod, pLazy, pInt, pBool, pWhile, pIf, pType, pComma :: Monad m => ParsecT [Token] u m Token
pSColon, pPower, pLE, pGE, pLT, pGT, pEQ, pNE, pOr   :: Monad m => ParsecT [Token] u m Token
pAnd, pAssign, pOB, pCB                              :: Monad m => ParsecT [Token] u m Token
-- pTrue, pFalse :: Monad m => ParsecT [Token] u m Token

pS      = genSymbolParser TkPlus
pM      = genSymbolParser TkMinus 
pNot    = genSymbolParser TkNot 
pMult   = genSymbolParser TkMult
pOP     = genSymbolParser TkOpenPar 
pCP     = genSymbolParser TkClosePar 
pQuote  = genSymbolParser TkQuote 
--pTrue   = genSymbolParser TkTrue 
--pFalse  = genSymbolParser TkFalse 
pMod    = genSymbolParser TkMod
pLazy   = genSymbolParser TkLazy 
pInt    = genSymbolParser TkInt
pBool   = genSymbolParser TkBool 
pWhile  = genSymbolParser TkWhile 
pIf     = genSymbolParser TkIf 
pType   = genSymbolParser TkType 
pComma  = genSymbolParser TkComma 
pSColon = genSymbolParser TkSemicolon 
pPower  = genSymbolParser TkPower 
pLE     = genSymbolParser TkLE 
pGE     = genSymbolParser TkGE 
pLT     = genSymbolParser TkLT
pGT     = genSymbolParser TkGT  
pEQ     = genSymbolParser TkEQ 
pNE     = genSymbolParser TkNE 
pOr     = genSymbolParser TkOr
pAnd    = genSymbolParser TkAnd 
pAssign = genSymbolParser TkAssign 
pOB     = genSymbolParser TkOpenBrace 
pCB     = genSymbolParser TkCloseBrace 


nonAssocParser :: Monad m => ParsecT [Token] u m Token -> ParsecT [Token] u m Token
nonAssocParser p = p >>= \e -> notFollowedBy p >> return e

pP0 :: SP m Expr
pP0 = eval <$> pP1 <*> many ((,) <$> p0Ops <*> pP1)
    where
        p0Ops = pAnd
        a = many ((,) <$> p0Ops <*> pP1)

pP1 :: SP m Expr
pP1 = eval <$> pP2 <*> many ((,) <$> p1Ops <*> pP2)
    where
        p1Ops = pOr

pP2 :: SP m Expr
pP2 = eval <$> pP3 <*> many ((,) <$> p2Ops <*> pP3)
    where
        p2Ops = nonAssocParser pEQ <|> 
                nonAssocParser pNE

pP3 :: SP m Expr
pP3 = eval <$> pP4 <*> many ((,) <$> p3Ops <*> pP4)
    where
        p3Ops = nonAssocParser pGT <|> 
                nonAssocParser pLT <|>
                nonAssocParser pGE <|>
                nonAssocParser pLE 

pP4 :: SP m Expr
pP4 = eval <$> pP5 <*> many ((,) <$> p4Ops <*> pP5)
    where
        p4Ops = pS <|> pM


pP5 :: SP m Expr
pP5 = eval <$> pP6 <*> many ((,) <$> p5Ops <*> pP6)
    where
        p5Ops = pMult <|> pMod

pP6 :: SP m Expr
pP6 = f <$> A.optional uOP <*> (eval <$> pP7 <*> many ((,) <$> p6Ops <*> pP7))
    where
        p6Ops = pPower 
        uOP = pNot <|> pM <|> pS
        f Nothing x       = x
        f (Just tk ) x =  case tk of
            TkNot   -> Not x
            TkPlus  -> Pos x
            TkMinus -> Negate x
            _       -> error "Imposible case"

pP7 :: SP m Expr
pP7 = isFapp <|> isNum <|> isId <|> isBool <|> between pOP pCP pE <|> between pQuote pQuote pE

parse' :: String -> String
parse' s = case manyToken s of
    Left err -> "lex error"
    Right tks -> case runParserT pP0 () "" tks of
        Identity  (Left e)    -> show e
        Identity (Right expr) -> toPrettyS (E expr)

pE :: SP m Expr
pE  = eval <$> term <*> many ((,) <$> (pS <|> pM) <*> term)



term :: SP m Expr
term = eval <$> factor <*> many ((,) <$> (pMult <|> pMult) <*> factor)


eval :: Expr -> [(Token, Expr)] -> Expr
eval e []               = e
eval e ((TkPlus,t):ts)  = eval (Plus e t) ts
eval e ((TkMinus,t):ts) = eval (Minus e t) ts
eval e ((TkMult,t):ts)  = eval (Times e t) ts
eval e ((TkPower,t):ts) = Pow e (eval t ts )
eval e ((TkLT,t):ts)    = eval (LT e t) ts
eval e ((TkGT,t):ts)    = eval (GT e t) ts
eval e ((TkLE,t):ts)    = eval (LE e t) ts 
eval e ((TkGE,t):ts)    = eval (GE e t) ts
eval e ((TkNE,t):ts)    = eval (NEQ e t) ts
eval e ((TkAnd,t):ts)   = And e (eval t ts)
eval e ((TkOr,t):ts)    = Or e  (eval t ts)
eval e ts               = error "aun no definido."

factor :: SP m Expr
factor = f <$> A.optional pM <*> (isNum <|> between pOP pCP pE <|> between pQuote pQuote pE)
    where
        f Nothing x = x
        f _ x       = Negate x







