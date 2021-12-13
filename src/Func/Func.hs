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

iST :: STable
iST = STable{getTable=t} 
    where
        
        f0 = Var "gcd"
        d0 = IdState (Fun [LInt, LInt ] LInt ) f0 undefined (gcd' <$> get)

        f1 = Var "fibo"
        d1 = IdState (Fun [LInt ] LInt) f1 undefined (fibo' <$> get)

        f2 = Var "irandom"
        d2 = IdState (Fun [LInt] LInt ) f2 undefined (mkIC . irandom' <$> get)

        f3 = Var "now"
        d3 = IdState (Fun [] LInt) f3 undefined (mkIC . (\ _ -> now ())  <$> get)
        
        f4 = Var "reset"
        d4 = IdState (Fun [] Void) f4 undefined reset

        t = Map.fromList [(f0,d0),(f1,d1),(f2,d2),(f3,d3), (f4,d4)]


reset :: State STable Expr 
reset = put iST >> return (mkBC True)

{-# NOINLINE irandom #-}
-- | Random number generator
irandom :: Int -> IO Int
irandom n
    | n > 0 = randomRIO (0, n - 1)
    | otherwise = error "The upper limit must be at least zero"


irandom' :: STable -> Int
irandom' st = unsafePerformIO $ irandom arg1
    where
        prod =  traverse evalArithm  $ getArgList "irandom" 1  st
        Just [arg1] = evalStateT prod st

-- | Fibonacci calculator
fibo :: Int -> Int
fibo n = fib !! n
    where
        fib = 0 : 1 : [a + b | (a, b) <- zip fib (tail fib)]

fibo' :: STable -> Expr
fibo' st =  mkIC (fibo arg1)
    where
        prod =  traverse evalArithm  $ getArgList "fibo" 1  st
        Just [arg1] = evalStateT prod st

gcd' :: STable -> Expr
gcd' st =  mkIC (gcd arg1 arg2)
    where
        prod =  traverse evalArithm  $ getArgList "gcd" 2  st
        Just [arg1,arg2] = evalStateT prod st


-- | Milliseconds elapsed since January 1, 1970 at midnight UTC time 
now :: () -> Int
{-# NOINLINE now #-}
now () = unsafePerformIO $ round <$> getPOSIXTime 
    where
        aux = [round <$> getPOSIXTime]
        a2 = sequence aux 
        a3 = unsafePerformIO a2 
        a4 = head a3