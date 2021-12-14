{-# OPTIONS_GHC -fno-full-laziness #-}
module Func.Func where
{- |
Module      : Func
Description : Provisional Module that holds the functions for the execution enviroment.
Maintainer  : 15-11139@usb.ve, 16-10400@usb.ve 17-10538@usb.ve
Stability   : experimental
Portability : POSIX

-}
import AST.AST
import ValidT.ValidT
import Data.Time.Clock.POSIX
import Control.Monad.State.Strict
import System.Random
import           Prelude          hiding (EQ, GT, LT)
import qualified Data.Map as Map
import System.IO.Unsafe
import Debug.Trace
import Data.Functor.Identity ( Identity(Identity) )
import Control.Monad.Morph

iST :: STable
iST = STable{getTable=t} 
    where
        
        f0 = Var "gcd"
        d0 = IdState (Fun [LInt, LInt ] LInt ) f0 undefined (get >>= (lift . gcd'))

        f1 = Var "fibo"
        d1 = IdState (Fun [LInt ] LInt) f1 undefined (get >>= (lift . fibo'))

        f2 = Var "irandom"
        d2 = IdState (Fun [LInt] LInt ) f2 undefined (get >>= (lift . fmap mkIC . irandom'))

        f3 = Var "now"
        d3 = IdState (Fun [] LInt) f3 undefined (mkIC . (\ _ ->  now ())  <$> get)
        
        f4 = Var "reset"
        d4 = IdState (Fun [] Void) f4 undefined reset

        sp0 = Var "type"
        d5 = IdState (Fun [Any] Any) sp0 undefined (type'' <$> get)

        sp1 = Var "ltype"
        d6 = IdState (Fun [Any] Any) sp1 undefined (get >>= (lift . lType'))

        sp2 = Var "cvalue"
        d7 = IdState (Fun [Any] Any) sp2 undefined (get >>= (lift . cvalue'))

        sp3 = Var "if"
        d8 = IdState (Fun [LBool,Any,Any] Any) sp3 undefined (get >>= (lift . if''))

        t = Map.fromList [(f0,d0),(f1,d1),(f2,d2),(f3,d3), (f4,d4), (sp0,d5), (sp1,d6), (sp2,d7),(sp3,d8)]


if' :: STable -> Expr -> Expr -> Expr -> Either String Expr
if' st b e e' = case evalStateT (evalBool b) st of
    Right b' -> if b' then evalStateT (eval' e) st else evalStateT (eval' e') st
    Left e   -> Left e

if'' :: STable -> Either String Expr 
if'' st = if' st arg1 arg2 arg3
    where
        [arg1,arg2,arg3] =  getLazyArgList "if" 3  st

type' :: Expr -> STable  -> Expr 
type' (Var "type")   _ =  Var "type"
type' (Var "ltype")  _ =  Var "type"
type' (Var "if")     _ =  Var "any"
type' (Var "cvalue") _ =  Var "any"
type' e              st = Var . show . fromRight $ validate (E e) st
    where
        fromRight (Right a) = a 

type'' :: STable  -> Expr 
type'' st = type' arg1 st 
    where
        [arg1] =   getLazyArgList "type" 1  st

lType :: Expr -> StateT STable Maybe LipsT
lType = getExpLType

lType' :: STable  -> Either String Expr 
lType' st = case evalStateT (Func.Func.lType arg1) st of
    Just t   -> Right . Var $ show t
    Nothing  -> Left "Error! Expression doesn't have an L-Type"
    where
        [arg1] =   getLazyArgList "ltype" 1  st

cvalue :: Expr -> StateT STable Maybe Expr
cvalue = getExpCValue


cvalue' :: STable  -> Either String Expr 
cvalue' st = case evalStateT (Func.Func.cvalue arg1) st of
    Just t   -> Right t
    Nothing  -> Left  "Error! Expression doesn't have a C-Value"
    where
        [arg1] =   getLazyArgList "cvalue" 1  st 

reset :: Monad m => StateT STable m Expr 
reset = put iST >> return (mkBC True)

{-# NOINLINE irandom #-}
-- | Random number generator
irandom :: Int -> IO (Either String Int)
irandom n
    | n > 0 = Right <$> randomRIO (0, n - 1)
    | otherwise = return $ Left "The upper limit must be at least zero"


irandom' :: STable -> Either String Int
irandom' st = case  traverse evalArithm  <$> getArgList "irandom" 1 st of
    Left errMsg -> Left errMsg
    Right exprs -> 
        let Right [arg1] = evalStateT exprs st 
        in  unsafePerformIO $ irandom arg1


-- | Fibonacci calculator
fibo :: Int -> Int
fibo n = fib !! n
    where
        fib = 0 : 1 : [a + b | (a, b) <- zip fib (tail fib)]

fibo' :: STable -> Either String Expr
fibo' st =  case  traverse evalArithm  <$> getArgList "fibo" 1 st of
    Left errMsg -> Left errMsg
    Right exprs -> 
        let Right [arg1] = evalStateT exprs st 
        in  Right . mkIC $ fibo arg1


gcd' :: STable -> Either String Expr
gcd' st = case  traverse evalArithm  <$> getArgList "gcd" 2 st of
    Left errMsg -> Left errMsg
    Right exprs -> 
        let Right [arg1,arg2] = evalStateT exprs st 
        in  Right . mkIC $ gcd arg1 arg2


-- | Milliseconds elapsed since January 1, 1970 at midnight UTC time 
now :: () -> Int
{-# NOINLINE now #-}
now () = unsafePerformIO $ round <$> getPOSIXTime 
    where
        aux = [round <$> getPOSIXTime]
        a2 = sequence aux 
        a3 = unsafePerformIO a2 
        a4 = head a3