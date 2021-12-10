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
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State.Strict

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
lookupType = lookupType' (\vName -> "Variable: '" ++ vName ++ "' has not been declared, assignment error!.")

-- | Updates a given identifier or yields an error if any error happens
updateIdentifier :: Identifier -> Expr ->  STable -> Either String STable
updateIdentifier vName expr STable{getTable=table} = case Map.lookup vName table of
    Nothing      -> Left $ "Variable: '" ++ vName ++ "' has not been declared, assignment error!."
    Just idState -> do 
        exprT <- type' expr 
        if exprT == lType idState
        then Right . STable $ push table IdState 
            { lType  = lType idState
            , lValue = lValue idState
            , cValue = expr
            , rValue = (`eval'` expr)
            }
        else Left $ "Variable: '" ++ vName ++ "' and the expression: '" ++ show expr ++ "' has different types." ++
            "\n" ++ vName ++ " type: " ++ show  (lType idState) ++ 
            "\n" ++ show expr ++ " type: " ++ show (type' expr)
    where
        push   = flip (Map.insert vName)

-- | Sets a given identifier or yields an error if any error happens
setIdentifier :: Identifier -> Expr -> LipsT -> STable -> Either String STable
setIdentifier vName expr vType STable{getTable=table} = case Map.lookup vName table of
    Just _  -> Left $ "Variable: '" ++ vName ++ "' is already declared! error."
    Nothing -> do
        exprT <- type' expr 
        if exprT == vType 
        then Right . STable $ Map.insert vName newId table
        else Left $ "Variable: '" ++ vName ++ "' and the expression: '" ++ show expr ++ "' has different types." ++
            "\n" ++ vName ++ " type: " ++ show  vType ++ 
            "\n" ++ show expr ++ " type: " ++ show (type' expr)
    where
        newId = IdState 
            { lType  = vType
            , lValue = Var vName 
            , cValue = expr
            , rValue = (`eval'` expr)
            }

-- | Dummy type
type' :: Expr -> Either String LipsT
type' = undefined 

-- | Dummy eval
eval' :: STable -> Expr -> Expr
eval' = undefined 

-- | Given an action, updates the state of the symbol table or yields an error.
updateActionTree :: Action -> StateT STable (Either String) ()
updateActionTree (Assignment vName nExpr)       = get >>= lift . updateIdentifier vName nExpr   >>= put 
updateActionTree (Declaration vType vName expr) = get >>= lift . setIdentifier vName expr vType >>= put
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