{- |
Module      : Type validator
Description : Provides necessary tools for lexing.
Maintainer  : 15-11139@usb.ve, 16-10400@usb.ve 17-10538@usb.ve
Stability   : experimental
Portability : POSIX
Main version
-}

module ValidT.ValidT where

import AST.AST
import Data.Maybe
import Data.Either
import HGrammar.HGrammar
import Lexer.Lexer
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Control.Monad.State.Strict
import           Prelude          hiding (EQ, GT, LT)
import Control.Monad.Morph
import Data.Functor.Identity ( Identity(Identity) ) 
import qualified PParser.PParser as PP
import Data.List (intercalate)
import Debug.Trace

-- | This function return de type of an action or a expression
-- | If the expression isn't valid will return a string with the
-- | error message
validate 
    :: S        -- ^ The action or expresion
    -> STable   -- ^ The table of symbols (where are saved the current variables)
    -> Either String LipsT -- ^ return (errorMessage or Type of the expression)
validate node tabla 
    | sisAction node = validateAction (sTakeAction node) tabla
    | sisSeq node = do 
        let (exp1,exp2) = takeSeq node 
        tipo1 <- validate exp1 tabla
        tipo2 <- validate exp2 tabla

        if sisE exp2 && exprIsReturn (sTakeExpr exp2) then
            return tipo2
        else 
            return tipo1
    | otherwise = validateExp (sTakeExpr node) tabla

-- This function validate an action (declaration of assignament)
validateAction 
    :: Action  -- ^ Action to be validated
    -> STable  -- ^ Table of symbols (where are saved the current variables)
    -> Either String LipsT -- ^ ( errorMessage or type of the action )
validateAction node tabla 
    -- In a declaration the types should be tha same
    | aisDeclaration node = do 
        let (tipo1,name,exp) = takeDeclaration node
        tipoExp <- validateExp exp tabla

        if containedT tipoExp tipo1 then
            return tipo1
        else
            Left ("Error: Declaracion invalida de "++name++" | la expresion a la derecha es de tipo "++show(tipoExp) ++ " y la variable debe ser "++show(tipo1) )
    -- In an assignament the type of the expression should be the same as the variable
    | aisAssignment node = do 
        let (name,exp) = takeAssignment node 
        tipo1 <- lookupType name tabla
        tipoExp <- validateExp exp tabla

        if containedT tipoExp tipo1 then
            return tipo1
        else
            Left ("Error: Declaracion invalida de "++name++" | Declarada como "++show(tipo1) ++ " pero estas asignando " ++ show(tipoExp) )
    | aisFDeclaration node = do 
        --    | FDeclaration LipsT String [(LipsT, String)] S
        let (typeReturn,name,arg,bodyF) = takeFDeclaration node
        let tabla2 = addArgsToTable arg tabla
        tipo <- validate bodyF tabla2
        if compareT typeReturn tipo then 
            return tipo
        else 
            Left ("Error: En la declaracion de la funcion '"++name++"' el tipo de la declaracion '"++show(typeReturn)++"' no coincide con el tipo retornado '"++show(tipo)++"'")
    | otherwise = Left "Error: A validate no se le puede pasar una secuencia de acciones"

-- This function validate an expression
validateExp 
    :: Expr     -- ^ Expression to be validated
    -> STable   -- ^ Table of symbols (where are saved the current variables)
    -> Either String LipsT -- ^ ( errorMessage or type of the expression )
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
    | exprIsReturn node = do 
        validateExp (takeReturn node) tabla 
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
        -- return (getTypeBinaryExpr node)
        if containedT tipo1 tipo2 then 
            -- Se tranforma el tipo al del operador
            return ( transformT tipo2 (getTypeBinaryExpr node) )
        else if containedT tipo2 tipo1 then 
            -- Se tranforma el tipo al del operador
            return ( transformT tipo1 (getTypeBinaryExpr node) )
        else
            Left ("Error: No se pueden aplicar el simbolo"++show(node)++" a estos tipos " ++ show(ex1) ++" es de tipo = "++show(tipo1) ++ " mientras que "++show(ex2) ++" es de tipo "++show(tipo2))
    | exprIsSeqE node = Left "La funcion validate no puede contener una secuencia de acciones/expresiones"
    | otherwise = Left "Error tipo de expresion no reconocido"

-- Functions signstures
getFunc :: String -> STable -> Either String (LipsT,[LipsT])
getFunc name st@STable{getTable=t} = case evalStateT (queryVal' (Var name)) st  of
    Left e       -> Left e
    Right idState  -> case lType idState of 
        (Fun args res) -> Right (res,args)
        _              -> Left (name ++ " no es de tipo funcion.")

compareT :: LipsT -> LipsT -> Bool
compareT Any _ = True
compareT _ Any = True
compareT a b   = a == b

-- Grafo Inverso de contencion
-- Es decir, Float c Int
nextType :: LipsT -> LipsT
nextType LInt = LBool
nextType LFloat = LInt
nextType (LLazy tipo) = tipo
nextType var = var 

-- containedT x y | te dice si 'x' esta contenido en 'y'
containedT :: LipsT -> LipsT -> Bool
containedT t1 t2
    | t1 == t2 = True
    | t2 == nextType t2 = False
    | otherwise = containedT t1 (nextType t2)

transformT :: LipsT -> LipsT -> LipsT
transformT (LLazy tipo) final = LLazy $ transformT tipo final
transformT x y = y 

-- Function to add a list of variables to a table
addArgsToTable :: [(LipsT, String)] -> STable -> STable
addArgsToTable [] t = t
addArgsToTable [x] cuTable = do 
    let (tipo,nombre) = x
    let newTable = setIdentifier nombre exp1 tipo cuTable
    newTable
addArgsToTable (x:xs) cuTable = do 
    let (tipo,nombre) = x
    let newTable = setIdentifier nombre exp1 tipo cuTable
    addArgsToTable xs newTable 

{-
mainValidate = do
    -- int Fun( int a, int b, lazy int c ){ a; b&&True; }
    -- int Fun( int a ){ a; }
    temp <- parse' "int Fun( int a, int b ){ false; b; a; }"
    return $ validate temp initialST

-}

--------------------------------

-- | An Identifier is just a String for now... (Can be changed into an expression or anything
-- later without compromising the code).
type Identifier = Expr

-- | The symbol table is just a Map.
data STable = STable { getTable :: Map Identifier IdState, autoCast :: Bool, levels :: [String] }

-- | The state of a variable holds all the information concerning that variab;e
data IdState = IdState  
    { lType  :: LipsT                  -- ^ The variable type
    , lValue :: Expr                   -- ^ The L-value of the variable
    , cValue :: Expr                   -- ^ The C-value of the variable
    , rValue :: StateT STable (Either String) Expr  -- ^ A way to calculate the rValue of a variable.
    }

-- | Aux function that modifies an entry in the Symbol table.
addIdState :: Monad m => Identifier -> IdState -> (IdState -> IdState -> IdState) -> StateT STable m ()
addIdState e eID f = modify g
    where
        g :: STable -> STable
        g  t@STable {getTable=_getTable} = t{getTable=Map.insertWith f e eID _getTable}

addLevel :: String -> StateT STable (Either String) ()
addLevel level = do
    t@STable {levels=_levels} <- get
    put t{levels=level:_levels}

dropLevel :: StateT STable (Either String) ()
dropLevel = do
    t@STable {levels=_levels} <- get
    put t{levels= tail _levels}

getPrefix :: Monad m => StateT STable m String 
getPrefix = concatMap (++"@") . levels <$> get

-- | Tries to Update the Symbol Table 
updateST :: S -> StateT STable (Either String) ()
updateST (A (Declaration t vName e)) = do 
    prefix <- getPrefix
    e' <- eval' e 
    let _e = case e of Lazy expr -> expr; expr -> expr 
    let vState = IdState t (Var (prefix ++ vName)) _e (eval' e')
    addIdState (Var (prefix ++ vName)) vState const 
updateST (A (Assignment vName expr)) = do
    prefix <- getPrefix 
    t <- queryVal' (Var vName)
    e' <- eval' expr
    let newState = IdState { lType  = lType t
        , lValue = lValue t
        , cValue = case expr of Lazy e -> e; e -> e
        , rValue = eval' e'
        }
    addIdState (Var (prefix ++ vName)) newState const 
updateST (A (FDeclaration ftype fname args body)) = do
    prefix  <- concatMap (++"@") .  (fname :) . levels <$> get
    prefix' <- getPrefix
    let
        fLType  = Fun (map fst args) ftype
        fLVal   = FApp (prefix' ++ fname) (map (Var . (prefix ++) . snd) args)
        fCValue = undefined  -- body
        fRValue = evalUF body
        newState = IdState {lType=fLType, lValue=fLVal, cValue=fCValue, rValue=fRValue}
    addIdState (Var $ prefix' ++ fname) newState const

updateST (A (SeqA a s)) = error "cannot happen"
updateST (E _) = return ()
updateST (Seq a b) = error "cannot happen2"

parse' s= case PP.parse' s of 
        Left (s,_) -> Left s
        Right a    -> Right a

validate' :: S -> StateT STable (Either String) ()
validate' s =  void <$> lift . validate s =<< get 
    where
        void f = () <$ f

process :: String -> StateT STable (Either String) [String]
process input = do
    let 
        seqList (Seq a b) = seqList a ++ seqList b
        seqList e         = [e]
    ast <-   lift $ parse' input
    traverse (\a -> validate' a >> process' a) $ seqList ast 

process' :: S -> StateT STable (Either String) String
process' ast@(A action) = updateST ast >>  (lift . Right) ("ACK: " ++ regenerateS ast)
process' ast@(E expr)   = eval' expr >>= \resExpr ->  lift . Right $ "OK: " ++ regenerateS ast ++ " ==> " ++ regenerateExpr resExpr


lookupType' :: String -> Identifier -> STable -> Either String LipsT
lookupType' ef var STable{getTable=table} = case Map.lookup var table of
    Nothing -> Left ef 
    Just t  -> Right $ lType t 


lookupType :: String -> STable -> Either String LipsT
lookupType vName = lookupType' ("La variable: '" ++ vName ++ "' no ha sido declarada, error de asignacion!.") (Var vName)

getExpLType :: Identifier  -> StateT STable Maybe LipsT
getExpLType (FApp v _) = getExpLType (Var v) 
getExpLType e = lift . funChecker . fmap lType . Map.lookup e . getTable =<< get
    where
        funChecker :: Maybe LipsT -> Maybe LipsT
        funChecker (Just (Fun _ _)) = Nothing 
        funChecker ml               = ml 

    
getExpType :: (Monad m, MonadFail m) => Expr -> StateT STable m LipsT
getExpType e = do 
    Right t <- validate (E e) <$> get 
    return t 

getExpCValue ::  Identifier -> StateT STable Maybe Expr
getExpCValue e = lift . getCValue . Map.lookup e . getTable =<< get 
    where
        getCValue :: Maybe IdState -> Maybe Expr
        getCValue (Just IdState{lType=Fun _ _}) = Nothing
        getCValue mid                           = cValue <$> mid 



getRValue :: Identifier -> STable -> Either String Expr
getRValue v st@STable{getTable=t} = evalStateT prod st
    where
        prod = rValue  (t ! v) 

-- | Sets a given identifier or yields an error if any error happens
setIdentifier' :: Identifier -> Expr -> LipsT -> STable -> STable
setIdentifier' vName expr vType t@STable{getTable=table} = t{getTable=Map.insert vName newId table}
    where
        newId = IdState 
            { lType  = vType
            , lValue = vName 
            , cValue = expr
            , rValue = eval' expr
            }

setIdentifier :: String -> Expr -> LipsT -> STable -> STable
setIdentifier vName = setIdentifier' (Var vName)


getAutocast :: Monad m => StateT STable m Bool 
getAutocast = autoCast <$> get 

-- | Dummy eval
eval' :: Expr -> StateT STable (Either String) Expr 
eval' ret@(Ret e) = return ret
eval' expr = eF <+> eF' <+> eB <+> eA <+> eL <+> (getAutocast >>= \b -> if b  then eBC <+> eAC else mzero )
    where
        (<+>) = mplus  
        eA  = mkIC <$> evalArithm False expr 
        eB  = mkBC <$> evalBool False expr
        eAC = mkIC <$> evalArithm True expr 
        eBC = mkBC <$> evalBool True expr
        eL  = evalLazy expr
        eF  = evalFApp' expr
        eF' = evalFApp' expr

eval :: S -> StateT STable (Either String) S
eval (E e) = E <$> eval' e
eval _     = lift (Left "")


eitherToMaybe :: Either b a -> Maybe a
eitherToMaybe (Right a) = Just a
eitherToMaybe (Left _)  = Nothing 

queryVal' :: Expr -> StateT STable (Either String) IdState 
queryVal' e = do 
    t <-  get
    name <- case e of Var n -> return n; FApp n _ -> return n; _ -> lift . Left $ "Cannot query"
    let  
        scopes :: [String] -> String -> [String]
        scopes xs n = scanr (\new acc -> new ++ "@" ++ acc ) n xs 
        
        lookupEnvs :: STable -> [String] -> String -> Either String IdState
        lookupEnvs STable {getTable=t} xs v =  f . foldl (\acc new -> acc `mplus` Map.lookup new t ) Nothing . map Var . scopes xs $ v
            where
                f (Just a) = Right a
                f Nothing  = Left $ "Variable: " ++ v ++ " does not exist in the current environment: " ++ show (Map.keys t)
        
        prefixes = levels t 
        
    lift (lookupEnvs t prefixes name) 

    -- rValue .  (! e) . getTable =<< get 
queryVal :: Expr -> StateT STable (Either String) Expr
queryVal e = queryVal' e >>=  rValue
    

evalArithm :: Bool -> Expr -> StateT STable (Either String) Int
evalArithm p (C (NumConstant  n)) = return n
evalArithm True (C (BConstant b)) = if b then return 1 else return 0
evalArithm p (Negate e)  = (* (-1)) <$> evalArithm p e
evalArithm p (Pos e)     = evalArithm p  e
evalArithm p (Plus a b)  = (+) <$> evalArithm p a <*> evalArithm p b
evalArithm p (Minus a b) = (-) <$> evalArithm p a <*> evalArithm p b
evalArithm p (Mod a b)   = mod <$> evalArithm p a <*> evalArithm p b
evalArithm p (Times a b) = (*) <$> evalArithm p a <*> evalArithm p b
evalArithm p (Pow a b)   = (^) <$> evalArithm p a <*> evalArithm p b
evalArithm p v@(Var _)   = queryVal v  >>= evalArithm p
evalArithm p f@(FApp vName _) = (evalFApp f `mplus` evalFApp' f)  >>= evalArithm p
evalArithm p  e          = lift (Left $ "Error: la expresion " ++ show e ++ " NO es de tipo aritmetico")


bCast :: Bool -> Expr -> StateT STable (Either String) Bool
bCast True e  = (/=) <$> evalArithm True e <*> return 0 
bCast False _ = mzero 


evalBool :: Bool -> Expr -> StateT STable (Either String) Bool
evalBool p (C (BConstant  b))  = return b
evalBool p (Not e)    = not <$>  (evalBool p e  `mplus` bCast p e)
evalBool p (Or b b')  = (||) <$> (evalBool p b  `mplus` bCast p b) <*> (evalBool p b'  `mplus` bCast p b')
evalBool p (And b b') = (&&) <$> (evalBool p b  `mplus` bCast p b) <*> (evalBool p b'  `mplus` bCast p b')
evalBool p (EQ a b)   = (==) <$> eval' a <*>  eval' b
evalBool p (NEQ a b)  = (/=) <$> eval' a <*>  eval' b
evalBool p (LT a b)   = (<)  <$> evalArithm p a <*> evalArithm p b
evalBool p (GT a b)   = (>)  <$> evalArithm p a <*> evalArithm p b
evalBool p (LE a b)   = (<=) <$> evalArithm p a <*> evalArithm p b
evalBool p (GE a b)   = (>=) <$> evalArithm p a <*> evalArithm p b
evalBool p v@(Var _)  = queryVal v >>= evalBool p
evalBool p f@(FApp vName _) = (evalFApp f `mplus` evalFApp' f) >>= evalBool p 
evalBool _ e          = lift (Left $ "Error: la expresion " ++ show e ++ " NO es de tipo booleano o casteable a booleano")


evalLazy :: Expr -> StateT STable (Either String) Expr
evalLazy (Lazy e)    = return e
evalLazy c@(C _)     = return c
evalLazy v@(Var vName) = hoist f $ getExpCValue v
    where
        f (Just a) = Right a
        f Nothing  = Left $ "La expresion asociada a: '" ++ vName ++ "' NO es de tipo lazy."
evalLazy   e      = lift (Left $ "Error: la expresion " ++ show e ++ " NO es de tipo lazy")


evalFApp :: Expr -> StateT STable (Either String) Expr
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

    lift $ f st{getTable=t'} 
    
evalFApp e              = lift (Left $ "Error: la expresion " ++ show e ++ " NO es de tipo funcion")

evalFApp' :: Expr -> StateT STable (Either String) Expr
evalFApp' aux@(FApp fName argsVal) = do
    addLevel fName
    prefix <- getPrefix
    st@STable {getTable=t} <- get
    
    fState <- queryVal' (Var fName)
    let 
        (FApp _ args')  =  lValue fState
        args = (\(Var name) -> Var $ name) <$> args'
        (Fun argsT _)  = lType fState

        f = getRValue (Var fName)

        serializeArg (name, arg, t) = IdState 
            { lType  = t
            , lValue = name
            , cValue = arg
            , rValue = eval' arg
            }
        
        serializeArgs   = serializeArg <$> zip3 args argsVal argsT
        t'              = foldr (uncurry Map.insert) t (args `zip` serializeArgs)
        fromRight (Right a) = a
    res <-   lift $ f st{getTable= t'} 
    put st
    dropLevel
    return res 
evalFApp' e              = lift (Left $ "Error: la expresion " ++ show e ++ " NO es de tipo funcion")

evalUF :: S -> StateT STable (Either String) Expr
evalUF (Seq ast@(A action) b) = updateST ast >> evalUF b
evalUF (Seq ast@(E expr) b)   = do
    e <- eval' expr
    case e of 
        Ret e' -> eval' e'
        _      -> evalUF b
evalUF (Seq (Seq a a') b)   = evalUF $ Seq a (Seq a' b)
evalUF ast@(A action) = updateST ast >> return Skip 
evalUF (E (Ret e))    = eval' e
evalUF (E expr)       = eval' expr 


getArgList :: String -> Int -> STable -> Either String [Expr]
getArgList fName nArgs st@STable{getTable=t} = sequence $ ($ st) . getRValue  <$> argsNames
    where
        argsNames = [ Var $ fName ++ "@" ++ show n | n <- [1..nArgs]]

getLazyArgList :: String -> Int -> STable -> [Expr]
getLazyArgList fName nArgs st@STable{getTable=t} =  cValue . (t !)  <$> argsNames
    where
        argsNames = [ Var $ fName ++ "@" ++ show n | n <- [1..nArgs]]


dummyPlus :: Int -> Int -> Int -> Int
dummyPlus a b c = a + b + c

dummyPlus' :: STable -> Either String Expr 
dummyPlus' st = case  traverse (evalArithm True)  <$> getArgList "dummyPlus" 3 st of
    Left errMsg -> Left errMsg
    Right exprs -> 
        let Right [arg1,arg2,arg3] = evalStateT exprs st 
        in  Right $ mkIC (dummyPlus arg1 arg2 arg3)




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
initialST = STable{getTable=t, autoCast=True, levels=[]} 
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
            , rValue = get >>= (lift . dummyPlus')
            }
        
        t = Map.fromList [(v1,d1),(v2,d2),(v3,d3),(v4,d4)]
    

test1,test2,test3,test4,test5,test6,test7 :: StateT STable (Either String) Expr
test1 = eval' exp1
test2 = eval' exp2
test3 = eval' exp3
test4 = eval' exp4
test5 = eval' exp5
test6 = eval' exp6
test7 = eval' exp7

tests = mapM (print . flip evalStateT initialST) [test1,test2,test3,test4,test5,test6,test7]
