module STable.STable where
{- |
Module      : STable
Description : Provisional Module that holds the L/C-Value Functions and the Symbol Table definition.
Maintainer  : 15-11139@usb.ve, 16-10400@usb.ve 17-10538@usb.ve
Stability   : experimental
Portability : POSIX

-}
import AST.AST
import           Prelude          hiding (EQ, GT, LT)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Control.Monad.State.Strict
import Control.Monad

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
eval' st expr = fromJust $  eA <+> eB <+> eL <+> eF 
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
evalLazy st (Lazy e) = Just $ eval' st e
evalLazy _    _      = Nothing 


validateExp :: Expr -> STable -> Either String LipsT
validateExp node tabla = undefined 

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






-- | Given an action, updates the state of the symbol table or yields an error.
updateActionTree :: Action -> StateT STable (Either String) ()
updateActionTree (Assignment vName nExpr)       = get >>= lift . (return . updateIdentifier vName nExpr)   >>= put 
updateActionTree (Declaration vType vName expr) = get >>= lift . (return . setIdentifier vName expr vType) >>= put
updateActionTree (SeqA a (A b)) = updateActionTree a >> updateActionTree b
updateActionTree (SeqA a (E b)) = updateActionTree a >> updateExprTree   b

-- | Given an expresion, updates the state of the symbol table or yields an error.
-- Currently it does nothing since expressions can't mutate variables.
updateExprTree :: Expr -> StateT STable (Either String) ()
updateExprTree _ = return ()


updateSTree :: S -> StateT STable (Either String) ()
updateSTree (A a) = updateActionTree a
updateSTree (E e) = updateExprTree   e

{-
updateExprTree :: Expr -> StateT STable (Either String) ()
updateExprTree (Negate expr) = updateExprTree expr
updateExprTree (Pos expr)    = updateExprTree expr
updateExprTree (Not expr)    = updateExprTree expr
updateExprTree (Plus a b)    = updateExprTree a >> updateExprTree b
updateExprTree (Minus a b)   = updateExprTree a >> updateExprTree b
updateExprTree (Mod a b)     = updateExprTree a >> updateExprTree b
updateExprTree (Times a b)   = updateExprTree a >> updateExprTree b
updateExprTree (Pow a b)     = updateExprTree a >> updateExprTree b
-}