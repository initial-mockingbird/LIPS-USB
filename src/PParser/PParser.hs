module PParser.PParser where


import           Lexer.Lexer
import Text.Parsec
import Text.Parsec.Combinator

import Data.Functor.Identity
import AST.AST
import qualified Control.Applicative as A (optional)

type TPos = Token



type SP m a = ParsecT [TPos] m Identity a

isId :: Parsec [TPos] u Expr
isId = tokenPrim  show g f
    where
        f (TkId x) = Just $ Var x
        f _          = Nothing
        g pos _ _    = incSourceColumn pos 1

isNum :: Monad m => ParsecT [TPos] u m Expr
isNum = tokenPrim show g f
    where
        f (TkNum n) = Just $ C $ NumConstant n
        f _           = Nothing
        g pos _  _    = incSourceColumn pos 1

parseSum :: Parsec [TPos] u (Expr -> Expr -> Expr)
parseSum = tokenPrim show g f
    where
        f TkPlus      = Just $ \x y -> Plus x y
        f _           = Nothing
        g pos _  _    = incSourceColumn pos 1

parseMinus :: Parsec [TPos] u (Expr -> Expr -> Expr)
parseMinus = tokenPrim show g f
    where
        f TkMinus     = Just $ \x y -> Minus x y
        f _           = Nothing
        g pos _  _    = incSourceColumn pos 1


parseMul :: Parsec [TPos] u (Expr -> Expr -> Expr)
parseMul = tokenPrim show g f
    where
        f TkMult      = Just $ \x y -> Times x y
        f _           = Nothing
        g pos _  _    = incSourceColumn pos 1




pS :: Parsec [TPos] u TPos
pS = tokenPrim show g f
    where
        f TkPlus      = Just TkPlus
        f _           = Nothing
        g pos _  _    = incSourceColumn pos 1

pM :: Monad m => ParsecT [TPos] u m TPos
pM = tokenPrim show g f
    where
        f TkMinus     = Just TkMinus
        f _           = Nothing
        g pos _  _    = incSourceColumn pos 1

pMult :: Monad m => ParsecT [TPos] u m TPos
pMult = tokenPrim show g f
    where
        f TkMult      = Just TkMult
        f _           = Nothing
        g pos _  _    = incSourceColumn pos 1

pOP :: Monad m => ParsecT [TPos] u m TPos
pOP = tokenPrim show g f
    where
        f TkOpenPar      = Just TkOpenPar
        f _           = Nothing
        g pos _  _    = incSourceColumn pos 1

pCP :: Monad m => ParsecT [TPos] u m TPos
pCP = tokenPrim show g f
    where
        f TkClosePar       = Just TkClosePar
        f _           = Nothing
        g pos _  _    = incSourceColumn pos 1

pQuote :: Monad m => ParsecT [TPos] u m TPos
pQuote = tokenPrim show g f
    where
        f TkQuote     = Just TkQuote 
        f _           = Nothing
        g pos _  _    = incSourceColumn pos 1

pE :: SP m Expr
pE  = eval <$> term <*> many ((,) <$> (pS <|> pM) <*> term)



term :: SP m Expr
term = eval <$> factor <*> many ((,) <$> (pMult <|> pMult) <*> factor)


eval :: Expr -> [(TPos, Expr)] -> Expr
eval e []               = e
eval e ((TkPlus,t):ts)  = eval (Plus e t) ts
eval e ((TkMinus,t):ts) = eval (Minus e t) ts
eval e ((TkMult ,t):ts) = eval (Times e t) ts
eval e ts               = error "aun no definido."

factor :: SP m Expr
factor = f <$> A.optional pM <*> (isNum <|> between pOP pCP pE <|> between pQuote pQuote pE)
    where
        f Nothing x = x
        f _ x       = Negate x







