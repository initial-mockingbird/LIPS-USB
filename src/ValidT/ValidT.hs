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
import STable.STable
import qualified Data.Map as Map

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
