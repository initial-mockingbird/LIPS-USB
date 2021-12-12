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

-- | Random number generator
irandom :: Int -> IO Int
irandom n
    | n < 0 = randomRIO (0, n - 1)
    | otherwise = error "The upper limit must be at least zero"

irandom' :: STable -> IO Int
irandom' st = irandom arg1
    where
        Just [arg1] = traverse (evalArithm st) $ getArgList "irandom" 1 st

-- | Fibonacci calculator
fibo :: Int -> Int
fibo n = fib !! n
    where
        fib = 0 : 1 : [a + b | (a, b) <- zip fib (tail fib)]

fibo' :: STable -> Expr
fibo' st =  mkIC (fibo arg1)
    where
        Just [arg1] = traverse (evalArithm st) $ getArgList "fibo" 1 st

-- | Greatest common divisor calculator
fgcd :: Int -> Int -> Int
fgcd a b = Prelude.gcd a b

gcd' :: STable -> Expr
gcd' st =  mkIC (fgcd arg1 arg2)
    where
        Just [arg1, arg2] = traverse (evalArithm st) $ getArgList "fgcd" 2 st

-- | Milliseconds elapsed since January 1, 1970 at midnight UTC time 
now :: IO Int
now = round `fmap` getPOSIXTime