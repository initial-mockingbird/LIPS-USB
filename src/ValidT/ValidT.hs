{- |
Module      : Type validator
Description : Provides necessary tools for lexing.
Maintainer  : 15-11139@usb.ve, 16-10400@usb.ve 17-10538@usb.ve
Stability   : experimental
Portability : POSIX
-}

module ValidT.ValidT where

import AST.AST
import Data.Maybe
import Data.Either
import HGrammar.HGrammar
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Control.Monad.State.Strict
import           Prelude          hiding (EQ, GT, LT)
import Control.Monad.Morph
import Data.Functor.Identity ( Identity(Identity) ) 

validate :: S -> STable -> Either String LipsT
validate node tabla 
    | sisAction node = validateAction (sTakeAction node) tabla
    | otherwise = validateExp (sTakeExpr node) tabla

validateAction :: Action -> STable -> Either String LipsT
validateAction node tabla 
    | aisDeclaration node = do 
        let (tipo1,name,exp) = takeDeclaration node
        tipoExp <- validateExp exp tabla

        if tipo1 `compareT` tipoExp then
            return tipo1
        else
            Left ("Declaracion invalida de "++name++" | la expresion a la derecha es de tipo "++show(tipoExp) ++ " y la variable debe ser "++show(tipo1) )
    | aisAssignment node = do 
        let (name,exp) = takeAssignment node 
        tipo1 <- lookupType name tabla
        tipoExp <- validateExp exp tabla

        if tipo1 `compareT` tipoExp then
            return tipo1
        else
            Left ("Declaracion invalida de "++name++" | Declarada como "++show(tipo1) ++ " pero estas asignando " ++ show(tipoExp) )
    | otherwise = Left "A validate no se le puede pasar una secuencia de acciones"

validateExp :: Expr -> STable -> Either String LipsT
validateExp node tabla
    -- Base cases
    | exprIsC node = Right $ takeTypeC node
    | exprIsLazy node = do
        tipo <- validateExp (takeLazy node) tabla
        return (LLazy tipo)
    | exprIsFApp node = do 
        let (name, lista) = takeFApp node 
        tiposParam <- traverse (`validateExp` tabla) lista
        (tipoF, tiposF) <- getFunc name tabla

        if length tiposParam == length tiposF then
            if and $ zipWith compareT tiposParam  tiposF then
                return tipoF
            else 
                Left ("Error en tipos de parametros en llamada a funcion "++name ++ " los correctos tipos son = "++show(tiposParam))
        else 
            Left ("Error en numero de parametros en llamada a funcion "++name)
    | exprIsVar node = do
        tipo <- lookupType (takeVar node) tabla
        return tipo 
    -- Unary expressions
    | exprIsUnary node = do
        tipo <- validateExp (exprUnaryTake node) tabla
        return (getTypeUnaryExpr node)
    -- Binary expression 
    | exprIsBinary node = do
        let (ex1,ex2) = takeFromBinaryExpr node 
        tipo1 <- validateExp ex1 tabla
        tipo2 <- validateExp ex2 tabla
        if tipo1 == tipo2 then
            return (getTypeBinaryExpr node)
        else if tipo1 /= tipo2 then 
            Left ("Tipos no coinciden " ++ show(ex1) ++" es de tipo = "++show(tipo1) ++ " mientras que "++show(ex2) ++" es de tipo "++show(tipo2))
        else
            Left ("No se puede aplicar el simbolo "++show(node) ++ " con "++ show(tipo1))
    | exprIsSeqE node = Left "La funcion validate no puede contener una secuencia de acciones/expresiones"
    | otherwise = Left "Error tipo de expresion no reconocido"

-- Definicion de firmas de funciones (return, tipos de los param)
getFunc :: String -> STable -> Either String (LipsT,[LipsT])
getFunc name st@STable{getTable=t} = case Map.lookup ( Var name) t of
    Nothing       -> Left ("No existe la funcion "++name)
    Just idState  -> Right (res,args)
        where
            (Fun args res) = lType idState

compareT :: LipsT -> LipsT -> Bool
compareT Any _ = True
compareT _ Any = True
compareT a b   = a == b

{-
getFunc "irandom" = Right (LInt,[LInt])
getFunc "fibo" = Right (LInt,[LInt])
getFunc "gcd" = Right (LInt,[LInt,LInt])
getFunc "now" = Right (LInt,[])
getFunc name = Left ("No existe la funcion "++name)
-}
var =  C ( NumConstant 2 ) 
myVar =  C ( BConstant True ) 
tablaVacia = Map.empty
mainValidate = do
    myAST <- parse "3 || 1 + 2"
    let miTabla = setIdentifier "var" var LInt (STable tablaVacia)
    let miTabla2 = setIdentifier "myVar" myVar LBool miTabla
    return $ validate myAST miTabla2











--------------------------------

-- | An Identifier is just a String for now... (Can be changed into an expression or anything
-- later without compromising the code).
type Identifier = Expr

-- | The symbol table is just a Map.
newtype STable = STable { getTable :: Map Identifier IdState }

-- | The state of a variable holds all the information concerning that variab;e
data IdState = IdState  
    { lType  :: LipsT                  -- ^ The variable type
    , lValue :: Expr                   -- ^ The L-value of the variable
    , cValue :: Expr                   -- ^ The C-value of the variable
    , rValue :: State STable Expr  -- ^ A way to calculate the rValue of a variable.
    }

-- | Aux function that modifies an entry in the Symbol table.
addIdState :: Monad m => Identifier -> IdState -> (IdState -> IdState -> IdState) -> StateT STable m ()
addIdState e eID f = modify $ STable . Map.insertWith f e eID . getTable

-- | Tries to Update the Symbol Table 
updateST :: Monad m => S -> StateT STable m ()
updateST (A (Declaration t vName e)) = do 
    e' <- eval' e 
    let vState = IdState t (Var vName) e' (eval' e')
    addIdState (Var vName) vState const 
        
updateST (A (Assignment vName expr)) = do 
    t <- (! Var vName) . getTable <$> get
    e' <- eval' expr
    let newState = IdState { lType  = lType t
        , lValue = lValue t
        , cValue = e'
        , rValue = eval' e'
        }
    addIdState (Var vName) newState const 
    
updateST (A (SeqA a s)) = updateST (A a) >> updateST s
updateST (E _) = return ()

parse' s= case parse s of 
        Left (s,_) -> Left s
        Right a    -> Right a

validate' :: S -> StateT STable (Either String) ()
validate' s =  void <$> lift . validate s =<< get 
    where
        void f = () <$ f

process :: String -> StateT STable (Either String) String
process input = do
    ast <-  lift $ parse' input
    validate' ast
    case ast of
        (A action) -> do
            updateST ast
            lift . Right $ "ACK: " ++ show ast
        (E expr) -> do
            resExpr <- eval' expr
            lift . Right $ "OK: " ++ show ast ++ "==>" ++ show resExpr


lookupType' :: String -> Identifier -> STable -> Either String LipsT
lookupType' ef var STable{getTable=table} = case Map.lookup var table of
    Nothing -> Left ef 
    Just t  -> Right $ lType t 


lookupType :: String -> STable -> Either String LipsT
lookupType vName = lookupType' ("La variable: '" ++ vName ++ "' no ha sido declarada, error de asignacion!.") (Var vName)

getExpLType :: Identifier  -> StateT STable Maybe LipsT
getExpLType (FApp v _) = getExpLType (Var v) 
getExpLType e = lift . (fmap lType . Map.lookup e) . getTable =<< get

    
getExpType :: (Monad m, MonadFail m) => Expr -> StateT STable m LipsT
getExpType e = do 
    Right t <- validate (E e) <$> get 
    return t 

getExpCValue ::  Identifier -> StateT STable Maybe Expr
getExpCValue e = lift . (fmap cValue . Map.lookup e) . getTable =<< get 



getRValue :: Identifier -> STable -> Expr
getRValue v st@STable{getTable=t} = evalState prod st
    where
        prod = rValue  (t ! v) 

-- | Sets a given identifier or yields an error if any error happens
setIdentifier' :: Identifier -> Expr -> LipsT -> STable -> STable
setIdentifier' vName expr vType STable{getTable=table} = STable $ Map.insert vName newId table
    where
        newId = IdState 
            { lType  = vType
            , lValue = vName 
            , cValue = expr
            , rValue = eval' expr
            }

setIdentifier :: String -> Expr -> LipsT -> STable -> STable
setIdentifier vName = setIdentifier' (Var vName)

-- | Dummy eval
eval' :: Monad a => Expr -> StateT STable a Expr 
eval' expr = hoist (generalize . fromJust) $ eL <+> eA <+> eB <+> eF 
    where
        fromJust (Just a) = Identity a
        (<+>) = mplus  
        eA = mkIC <$> evalArithm expr 
        eB = mkBC <$> evalBool expr
        eL = evalLazy expr
        eF = evalFApp expr

eval :: S -> StateT STable Maybe S
eval (E e) = hoist generalize  $ E <$> eval' e
eval _     = lift Nothing 


queryVal :: Expr -> StateT STable Maybe Expr 
queryVal e = hoist generalize . rValue .  (! e) . getTable =<< get 

evalArithm :: Expr -> StateT STable Maybe Int
evalArithm (C (NumConstant  n)) = return n
evalArithm  (Negate e)  = (* (-1)) <$> evalArithm e
evalArithm  (Pos e)     = evalArithm  e
evalArithm  (Plus a b)  = (+) <$> evalArithm a <*> evalArithm b
evalArithm  (Minus a b) = (-) <$> evalArithm a <*> evalArithm b
evalArithm  (Mod a b)   = mod <$> evalArithm a <*> evalArithm b
evalArithm  (Times a b) = (*) <$> evalArithm a <*> evalArithm b
evalArithm  (Pow a b)   = (^) <$> evalArithm a <*> evalArithm b
evalArithm v@(Var _)    = do 
    (C (NumConstant n)) <- queryVal v  
    return  n
evalArithm  _           = lift Nothing 


bCast :: Expr -> StateT STable Maybe Bool
bCast e = (/=) <$> evalArithm e <*> return 0 

evalBool :: Expr -> StateT STable Maybe Bool
evalBool  (C (BConstant  b)) = return b
evalBool  (Not e)   = not <$> (evalBool e  `mplus` bCast e)
evalBool (Or b b')  = (||) <$> (evalBool b  `mplus` bCast b) <*> (evalBool b  `mplus` bCast b')
evalBool (And b b') = (&&) <$> (evalBool b  `mplus` bCast b) <*> (evalBool b  `mplus` bCast b')
evalBool (EQ a b)   = hoist generalize $ (==) <$> eval' a <*>  eval' b
evalBool (NEQ a b)  = hoist generalize $ (/=) <$> eval' a <*>  eval' b
evalBool (LT a b)   = (<)  <$> evalArithm a <*> evalArithm b
evalBool (GT a b)   = (>)  <$> evalArithm a <*> evalArithm b
evalBool (LE a b)   = (<=) <$> evalArithm a <*> evalArithm b
evalBool (GE a b)   = (>=) <$> evalArithm a <*> evalArithm b
evalBool v@(Var _)  = do 
    (C (BConstant n)) <- queryVal v  
    return  n
evalBool _ = lift Nothing 

evalLazy :: Expr -> StateT STable Maybe Expr
evalLazy (Lazy e)      = return e
evalLazy v@(Var vName) = do
    isState <- (! v) . getTable <$> get
    case lType isState of
        LLazy _ -> getRValue v <$> get
        _       -> lift Nothing 
evalLazy   _      = lift Nothing 


evalFApp :: Expr -> StateT STable Maybe Expr
evalFApp (FApp fName args) = do
    st@STable{getTable=t} <- get
    let
        f = getRValue (Var fName)

        serializeArg (name, arg) = IdState 
            { lType  = fromRight $ validateExp arg st
            , lValue = name
            , cValue = arg
            , rValue = eval' arg
            }
        
        serializeName n = fName ++ "@" ++ show n
        serializeNames  = Var <$> [serializeName n | n <- [1..(length args)]]
        serializeArgs   = serializeArg <$> zip serializeNames args
        t'              = foldr (uncurry Map.insert) t (serializeNames `zip` serializeArgs )
        fromRight (Right a) = a

    return $ f STable{getTable=t'} 
    
evalFApp _                  = lift Nothing 



getArgList :: String -> Int -> STable -> [Expr]
getArgList fName nArgs st@STable{getTable=t} =  ($ st) . getRValue  <$> argsNames
    where
        argsNames = [ Var $ fName ++ "@" ++ show n | n <- [1..nArgs]]

getLazyArgList :: String -> Int -> STable -> [Expr]
getLazyArgList fName nArgs st@STable{getTable=t} =  cValue . (t !)  <$> argsNames
    where
        argsNames = [ Var $ fName ++ "@" ++ show n | n <- [1..nArgs]]


dummyPlus :: Int -> Int -> Int -> Int
dummyPlus a b c = a + b + c

dummyPlus' :: STable -> Expr 
dummyPlus' st = mkIC (dummyPlus arg1 arg2 arg3)
    where
        prod =  traverse evalArithm  $ getArgList "dummyPlus" 3 st
        Just [arg1,arg2,arg3] = evalStateT prod st




mkIC :: Int -> Expr
mkIC = C . NumConstant 

mkBC :: Bool -> Expr
mkBC = C . BConstant 

exp1 :: Expr
exp1 = Minus (Times (mkIC 7) (mkIC 7)) (mkIC 7)

exp2 :: Expr
exp2 = LT (mkIC 6) (mkIC 7)

exp3 :: Expr 
exp3 = Not (mkBC True)

exp4 :: Expr
exp4 = Plus (Var "dani") (Var "angelito")

exp5 = Lazy (And (mkBC True) (mkBC False )) 

exp6 = Var "mari" 

exp7 = FApp "dummyPlus" [Var "dani", Var "angelito", mkIC 3]

initialST :: STable
initialST = STable{getTable=t} 
    where
        v1 = Var "dani"
        d1 = IdState { lType  = LInt 
            , lValue =  v1
            , cValue = mkIC 10
            , rValue = return $ mkIC 10
            }
        
        v2 = Var "angelito"
        d2 = IdState { lType  = LInt 
            , lValue = v2
            , cValue = mkIC (-15)
            , rValue = return $ mkIC (-15)
            }
        
        v3 = Var "mari"
        d3 = IdState { lType  = LLazy LInt 
            , lValue = v3
            , cValue = Lazy (Plus (mkIC 4) v2 )
            , rValue = eval' $ Lazy (Plus (mkIC 4) v2 )
            }

        v4 = Var "dummyPlus"
        d4 = IdState 
            { lType  = undefined 
            , lValue = v4
            , cValue = undefined 
            , rValue = dummyPlus' <$> get
            }
        
        t = Map.fromList [(v1,d1),(v2,d2),(v3,d3),(v4,d4)]
    
test1,test2,test3,test4,test5,test6,test7 :: State STable Expr
test1 = eval' exp1
test2 = eval' exp2
test3 = eval' exp3
test4 = eval' exp4
test5 = eval' exp5
test6 = eval' exp6
test7 = eval' exp7

tests = mapM (print . flip evalStateT initialST) [test1,test2,test3,test4,test5,test6,test7]
