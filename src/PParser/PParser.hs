module PParser.PParser where



import Lexer.Lexer hiding (isId)
import Text.Parsec
import Text.Parsec.Combinator

import Data.Functor.Identity
import AST.AST
import qualified Control.Applicative as A (optional)
import Prelude hiding (GT,LT,GE,LE, EQ)
import Text.Parsec.Error ( errorMessages, messageString )
import           Control.Monad.IO.Class (MonadIO (liftIO))


type SP m a = ParsecT [Token] m Identity a

isId :: Monad m => ParsecT [Token] u m Expr
isId = tokenPrim  show g f
    where
        f (TkId x)   = Just $ Var x
        f _          = Nothing
        g pos _ _    = incSourceColumn pos 1

isNum :: Monad m => ParsecT [Token] u m Expr
isNum = tokenPrim show g f
    where
        f (TkNum n)   = Just $ C $ NumConstant n
        f _           = Nothing
        g pos _  _    = incSourceColumn pos 1

isBool :: Monad m => ParsecT [Token] u m Expr
isBool = tokenPrim show g f
    where
        f TkTrue  = Just $ C $ BConstant True
        f TkFalse = Just $ C $ BConstant False 
        f _           = Nothing
        g pos _  _    = incSourceColumn pos 1 

isIntT :: Monad m => ParsecT [Token] u m LipsT  
isIntT = tokenPrim show g f
    where
        f TkInt   = Just LInt 
        f _           = Nothing
        g pos _  _    = incSourceColumn pos 1 

isBoolT :: Monad m => ParsecT [Token] u m LipsT  
isBoolT = tokenPrim show g f
    where
        f TkBool      = Just LBool 
        f _           = Nothing
        g pos _  _    = incSourceColumn pos 1 

isLazyT :: Monad m => ParsecT [Token] u m (LipsT -> LipsT)
isLazyT = tokenPrim show g f
    where
        f TkLazy      = Just LLazy
        f _           = Nothing
        g pos _  _    = incSourceColumn pos 1 

isIdOrFapp :: SP m Expr
isIdOrFapp = do
    var@(Var fName) <- isId
    let fapp' = do 
            pOP 
            fArgs <- pArgs <?> ("Error in function '" ++ fName ++ "': Functions must specify ALL their arguments.")
            notFollowedBy pComma <?> ("Error in function '" ++ fName ++ "': Functions must specify ALL their arguments.")
            pCP <?> ("Error in function '" ++ fName ++ "': Non closing Parenthesis found.") 
            return $ FApp fName fArgs
    
    fapp' <|> return var


pArgs :: SP m [Expr]
pArgs = sepBy pExpr pComma 


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


nonAssocEMessage :: String -> String
nonAssocEMessage op = "Association error: '" ++  op ++  "' is  non associative"

nonAssocCheck :: Expr -> SP m Expr
nonAssocCheck (EQ (EQ _ _) _)    = fail $ nonAssocEMessage "="
nonAssocCheck (EQ  _ (EQ _ _))   = fail $ nonAssocEMessage "="
nonAssocCheck (NEQ (NEQ _ _) _)  = fail $ nonAssocEMessage "<>"
nonAssocCheck (NEQ  _ (NEQ _ _)) = fail $ nonAssocEMessage "<>"
nonAssocCheck (GT (GT _ _) _)    = fail $ nonAssocEMessage ">"
nonAssocCheck (GT  _ (GT _ _))   = fail $ nonAssocEMessage ">"
nonAssocCheck (LT (LT _ _) _)    = fail $ nonAssocEMessage "<"
nonAssocCheck (LT  _ (LT _ _))   = fail $ nonAssocEMessage "<"
nonAssocCheck (GE (GE _ _) _)    = fail $ nonAssocEMessage ">="
nonAssocCheck (GE  _ (GE _ _))   = fail $ nonAssocEMessage ">="
nonAssocCheck (LE (LE _ _) _)    = fail $ nonAssocEMessage "<="
nonAssocCheck (LE  _ (LE _ _))   = fail $ nonAssocEMessage "<="
nonAssocCheck e                  = pure e



pAST :: SP m S
pAST = (E <$> pExpr <* (eof <?> "Parse error: Malformed Expression")) <|> (A <$> pAction)

pExpr :: SP m Expr
pExpr = pP0 >>= nonAssocCheck 

pP0 :: SP m Expr
pP0 = toETree <$> pP1 <*> many ((,) <$> p0Ops <*> pP1)
    where
        p0Ops = pAnd

pP1 :: SP m Expr
pP1 = toETree <$> pP2 <*> many ((,) <$> p1Ops <*> pP2)
    where
        p1Ops = pOr

pP2 :: SP m Expr
pP2 = toETree <$> pP3 <*> many ((,) <$> p2Ops <*> pP3)
    where
        p2Ops = pEQ <|> 
                pNE

pP3 :: SP m Expr
pP3 = toETree <$> pP4 <*> many ((,) <$> p3Ops <*> pP4)
    where
        p3Ops = pGT <|> 
                pLT <|>
                pGE <|>
                pLE 

pP4 :: SP m Expr
pP4 = toETree <$> pP5 <*> many ((,) <$> p4Ops <*> pP5)
    where
        p4Ops = pS <|> pM


pP5 :: SP m Expr
pP5 = toETree <$> pP6 <*> many ((,) <$> p5Ops <*> pP6)
    where
        p5Ops = pMult <|> pMod

pP6 :: SP m Expr
pP6 = f <$> A.optional uOP <*> (toETree <$> pP7 <*> many ((,) <$> p6Ops <*> pP7))
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
pP7 =  pParenE <|> pQuoteE <|> isNum <|> isBool <|> isIdOrFapp 
    where
        pParenE = between pOP    (pCP    <?> "Non closing parenthesis Found") pExpr       
        pQuoteE = between pQuote (pQuote <?> "Non closing Quote found Found") ( Lazy <$> pExpr)
        

parse' :: String -> String
parse' s = case manyToken s of
    Left err -> "lex error"
    Right tks -> case runParserT pAST () "" tks of
        Identity  (Left e)    -> show e
        Identity (Right expr) -> toPrettyS expr 

toETree :: Expr -> [(Token, Expr)] -> Expr
toETree e []               = e
toETree e ((TkPlus,t):ts)  = toETree (Plus e t) ts
toETree e ((TkMinus,t):ts) = toETree (Minus e t) ts
toETree e ((TkMult,t):ts)  = toETree (Times e t) ts
toETree e ((TkPower,t):ts) = Pow e (toETree t ts )
toETree e ((TkLT,t):ts)    = toETree (LT e t) ts
toETree e ((TkGT,t):ts)    = toETree (GT e t) ts
toETree e ((TkLE,t):ts)    = toETree (LE e t) ts 
toETree e ((TkGE,t):ts)    = toETree (GE e t) ts
toETree e ((TkNE,t):ts)    = toETree (NEQ e t) ts
toETree e ((TkAnd,t):ts)   = And e (toETree t ts)
toETree e ((TkOr,t):ts)    = Or e  (toETree t ts)
toETree e ((TkEQ, t):ts)   = toETree (EQ e t) ts
toETree e ts               = error "aun no definido."


pAction :: SP m Action 
pAction = pDeclaration <|> pAssignment

pDeclaration :: SP m Action 
pDeclaration = (f <$> pLType <*> pAssignment) <?> "Bad Declaration, format should follow: Lazy <Type> var := expr"
    where
        f t (Assignment v e) = Declaration t v e
        f _ _                = error "Impossible case"


pLType :: SP m LipsT 
pLType = (isBoolT <|> isIntT <|> isLazyT <*> pLType) <?> "Bad type initializator."

pAssignment :: SP m Action
pAssignment = f <$> isId <*> ((,) <$> (pAssign <?> "Bad assing symbol") <*> pExpr <* (eof <?> "Parse error: possible unmatched parenthesis/quotation" ))
    where
        f :: Expr -> (Token, Expr) -> Action
        f (Var v) (TkAssign, e ) = Assignment v e
        f _ _              = error "Bad parse"


parse ::  String -> ParsecT String u IO (Either (String,SourcePos) S) 
parse sn = do 
    eTokens <- manyToken <$> many anyToken
    pos     <- getPosition 
    case eTokens of
        Left xs -> do 
            let (err,col) = last xs
            setPosition $ setSourceColumn pos col
            fail err
        --Left _       -> error "Imposible case" 
        Right tks -> case runParserT pAST () sn tks of
            Identity (Left parseError) -> do
                let epos = errorPos parseError
                let emsg = messageString $ last $ errorMessages parseError
                liftIO $ putStrLn emsg
                setPosition epos
                return $ Left (emsg,epos)
            Identity (Right ast)   -> return $ Right ast 



