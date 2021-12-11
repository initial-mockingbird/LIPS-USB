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
import Parser.Tokens
import Lexer.Lexer
import HGrammar.HGrammar
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Control.Monad.State.Strict
import Control.Monad
import           Prelude          hiding (EQ, GT, LT)

validate :: S -> STable -> Either String LipsT
validate node tabla 
    | sisAction node = validateAction (sTakeAction node) tabla
    | otherwise = validateExp (sTakeExpr node) tabla

validateAction :: Action -> STable -> Either String LipsT
validateAction node tabla 
    | aisDeclaration node = do 
        let (tipo1,name,exp) = takeDeclaration node
        tipoExp <- validateExp exp tabla

        if tipo1 == tipoExp then
            return tipo1
        else
            Left ("Declaracion invalida de "++name++" | la expresion a la derecha es de tipo "++show(tipoExp) ++ " y la variable debe ser "++show(tipo1) )
    | aisAssignment node = do 
        let (name,exp) = takeAssignment node 
        tipo1 <- lookupType name tabla
        tipoExp <- validateExp exp tabla

        if tipo1 == tipoExp then
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
        (tipoF, tiposF) <- getFunc name 

        if length tiposParam == length tiposF then
            if tiposParam == tiposF then
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
        if tipo == (getTypeUnaryExpr node) then
            return tipo
        else
            Left ("No se pue||de aplicar este simbolo unario "++show(node)++" a expresion de tipo "++show(tipo))
    -- Binary expression 
    | exprIsBinary node = do
        let (ex1,ex2) = takeFromBinaryExpr node 
        tipo1 <- validateExp ex1 tabla
        tipo2 <- validateExp ex2 tabla
        if tipo1 == tipo2 && tipo1==(getTypeBinaryExpr node) then
            return tipo1
        else if tipo1 /= tipo2 then 
            Left ("Tipos no coinciden " ++ show(ex1) ++" es de tipo = "++show(tipo1) ++ " mientras que "++show(ex2) ++" es de tipo "++show(tipo2))
        else
            Left ("No se puede aplicar el simbolo "++show(node) ++ " con "++ show(tipo1))
    | exprIsSeqE node = Left "La funcion validate no puede contener una secuencia de acciones/expresiones"
    | otherwise = Left "Error tipo de expresion no reconocido"

-- Definicion de firmas de funciones (return, tipos de los param)
getFunc :: String -> Either String (LipsT,[LipsT])
getFunc "irandom" = Right (LInt,[LInt])
getFunc "fibo" = Right (LInt,[LInt])
getFunc "gcd" = Right (LInt,[LInt,LInt])
getFunc "now" = Right (LInt,[])
getFunc name = Left ("No existe la funcion "++name)

var = ( C ( NumConstant 2 ) )
myVar = ( C ( BConstant True ) )
tablaVacia = Map.empty
mainValidate = do
    myAST <- parse "lazy int var := '1 + 2'"
    let miTabla = setIdentifier "var" var LInt (STable tablaVacia)
    let miTabla2 = setIdentifier "myVar" myVar LBool miTabla
    return $ validate myAST miTabla2











--------------------------------

-- | An Identifier is just a String for now... (Can be changed into an expression or anything
-- later without compromising the code).
type Identifier = String

-- | The symbol table is just a Map.
newtype STable = STable { getTable :: Map Identifier IdState }

-- | The state of a variable holds all the information concerning that variab;e
data IdState = IdState  
    { lType  :: LipsT             -- ^ The variable type
    , lValue :: Expr              -- ^ The L-value of the variable
    , cValue :: Expr              -- ^ The C-value of the variable
    , rValue :: STable -> Expr    -- ^ A way to calculate the rValue of a variable.
    }


lookupType' :: (String -> String) -> Identifier -> STable -> Either String LipsT
lookupType' ef var STable{getTable=table} = case Map.lookup var table of
    Nothing -> Left $ ef var
    Just t  -> Right $ lType t 


lookupType :: Identifier -> STable -> Either String LipsT
lookupType = lookupType' (\vName -> "La variable: '" ++ vName ++ "' no ha sido declarada, error de asignacion!.")

-- | Updates a given identifier or yields an error if any error happens
updateIdentifier :: Identifier -> Expr ->  STable -> STable
updateIdentifier vName expr STable{getTable=table} = STable $ push table IdState 
        { lType  = lType idState
        , lValue = lValue idState
        , cValue = expr
        , rValue = (`eval'` expr)
        }
    where
        push    = flip (Map.insert vName)
        idState = table ! vName

-- | Sets a given identifier or yields an error if any error happens
setIdentifier :: Identifier -> Expr -> LipsT -> STable -> STable
setIdentifier vName expr vType STable{getTable=table} = STable $ Map.insert vName newId table
    where
        newId = IdState 
            { lType  = vType
            , lValue = Var vName 
            , cValue = expr
            , rValue = (`eval'` expr)
            }

-- | Dummy eval
eval' :: STable -> Expr -> Expr
eval' st expr = fromJust $ eL <+> eA <+> eB <+> eF 
    where
        (<+>) = mplus  
        eA = fmap (C . NumConstant) . evalArithm st $ expr 
        eB = fmap (C . BConstant) . evalBool st $ expr
        eL = evalLazy st expr
        eF = evalFApp st expr

        fromJust (Just a) = a

evalArithm :: STable -> Expr -> Maybe Int
evalArithm _ (C (NumConstant  n)) = Just n
evalArithm st@STable{getTable=t} (Var vName) = evalArithm st (rValue  (t ! vName) st )
evalArithm st (Negate e)  = (* (-1)) <$> evalArithm st e
evalArithm st (Pos e)     = evalArithm st e
evalArithm st (Plus a b)  = (+) <$> evalArithm st a <*> evalArithm st b
evalArithm st (Minus a b) = (-) <$> evalArithm st a <*> evalArithm st b
evalArithm st (Mod a b)   = mod <$> evalArithm st a <*> evalArithm st b
evalArithm st (Times a b) = (*) <$> evalArithm st a <*> evalArithm st b
evalArithm st (Pow a b)   = (^) <$> evalArithm st a <*> evalArithm st b
evalArithm _ _ = Nothing 


evalBool :: STable -> Expr -> Maybe Bool
evalBool _ (C (BConstant  b)) = Just b
evalBool st@STable{getTable=t} (Var vName) = evalBool st (rValue  (t ! vName) st )
evalBool st (Not e)    = not <$> evalBool st e
evalBool st (Or b b')  = (||) <$> evalBool st b <*> evalBool st b'
evalBool st (And b b') = (&&) <$> evalBool st b <*> evalBool st b'
evalBool st (EQ a b)   = return $ eval' st a == eval' st b
evalBool st (NEQ a b)  = return $ eval' st a /= eval' st b
evalBool st (LT a b)   = (<)  <$> evalArithm st (eval' st a) <*> evalArithm st (eval' st b)
evalBool st (GT a b)   = (>)  <$> evalArithm st (eval' st a) <*> evalArithm st (eval' st b)
evalBool st (LE a b)   = (<=) <$> evalArithm st (eval' st a) <*> evalArithm st (eval' st b)
evalBool st (GE a b)   = (>=) <$> evalArithm st (eval' st a) <*> evalArithm st (eval' st b)
evalBool _ _ = Nothing 

evalLazy :: STable -> Expr -> Maybe Expr
evalLazy st (Lazy e) = Just e
evalLazy st@STable{getTable=t} (Var vName) = case tp of
    LLazy _ -> return $ f st
    _       -> Nothing 
    where
        istate = t ! vName
        tp     = lType istate
        f      = rValue istate
evalLazy _    _      = Nothing 


evalFApp :: STable -> Expr -> Maybe Expr
evalFApp st@STable{getTable=t} (FApp fName args) = return $ f STable{getTable=t'} 
    where

        f = rValue $ t ! fName

        serializeArg (name, arg) = IdState 
            { lType  = fromRight $ validateExp arg st
            , lValue = Var name
            , cValue = arg
            , rValue = (`eval'` arg)
            }
        
        serializeName n = fName ++ "@" ++ show n
        serializeNames  = [serializeName n | n <- [1..(length args)]]
        serializeArgs = serializeArg <$> zip serializeNames args
        t'           = foldr (uncurry Map.insert) t (serializeNames `zip` serializeArgs )
        
        fromRight (Right a) = a
evalFApp _ _                  = Nothing 


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

initialST :: STable
initialST = STable{getTable=t} 
    where
        v1 = "dani"
        d1 = IdState { lType  = LInt 
            , lValue = Var v1
            , cValue = mkIC 10
            , rValue = (`eval'` mkIC 10)
            }
        
        v2 = "angelito"
        d2 = IdState { lType  = LInt 
            , lValue = Var v2
            , cValue = mkIC (-15)
            , rValue = (`eval'` mkIC (-15))
            }
        
        v3 = "mari"
        d3 = IdState { lType  = LLazy LInt 
            , lValue = Var v3
            , cValue = Lazy (Plus (mkIC 4) (Var v2))
            , rValue = (`eval'` Lazy (Plus (mkIC 4) (Var v2)))
            }
        
        t = Map.fromList [(v1,d1),(v2,d2),(v3,d3)]
    

test1 = eval' initialST exp1
test2 = eval' initialST exp2
test3 = eval' initialST exp3
test4 = eval' initialST exp4
test5 = eval' initialST exp5
test6 = eval' initialST exp6


tests = mapM print [test1,test2,test3,test4,test5,test6]
