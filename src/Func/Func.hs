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
iST = STable{getTable=t, autoCast=True, levels = []} 
    where
        
        invalidCVal = E $ EString "Predefined functions can't have a representable cvalue"

        f0 = Var "gcd"
        d0 = IdState (Fun [LInt, LInt ] LInt ) (FApp "gcd" [Var "gcd@x", Var "gcd@y"])  invalidCVal gcd'

        f1 = Var "fibo"
        d1 = IdState (Fun [LInt ] LInt) (FApp "fibo" [Var "fibo@n"])  invalidCVal fibo'

        f2 = Var "irandom"
        d2 = IdState (Fun [LInt] LInt ) (FApp "irandom" [Var "irandom@n"]) invalidCVal irandom'

        f3 = Var "now"
        d3 = IdState (Fun [] LInt) (FApp "now" []) invalidCVal (mkIC . (\ _ ->  now ())  <$> get)
        
        f4 = Var "reset"
        d4 = IdState (Fun [] Void) (FApp "reset" []) invalidCVal reset

        f5 = Var "logB2"
        d5 = IdState (Fun [LInt ] LInt) (FApp "logB2" [Var "logB2@n"]) invalidCVal logB2'

        f6 = Var "toBinary"
        d6 = IdState (Fun [LInt ] LInt) (FApp "toBinary" [Var "toBinary@n"]) invalidCVal toBinary'

        sp0 = Var "type"
        d7 = IdState (Fun [Any] Any) (FApp "type" [Var "type@expr"]) invalidCVal type''

        sp1 = Var "ltype"
        d8 = IdState (Fun [Any] Any) (FApp "ltype" [Var "ltype@expr"]) invalidCVal lType'

        sp2 = Var "cvalue"
        d9 = IdState (Fun [Any] Any) (FApp "cvalue" [Var "cvalue@expr"]) invalidCVal cvalue'

        sp3 = Var "if"
        d10 = IdState (Fun [LBool,Any,Any] Any) (FApp "if" [Var "condition", Var "ifTrue", Var "ifFalse"]) invalidCVal if''

        t = Map.fromList [(f0,d0),(f1,d1),(f2,d2),(f3,d3), (f4,d4), (f5,d5), (f6,d6), (sp0,d7), (sp1,d8), (sp2,d9),(sp3,d10)]


if'' :: StateT STable (Either String) Expr
if'' = do 
    aC   <- autoCast <$> get
    st   <- getTable  <$> get
    args <-  getLazyArgList "if" 
    case args of
        [arg1,arg2,arg3] -> do
            b <- evalBool aC arg1
            if b then eval' arg2 else eval' arg3
        _                -> lift . Left $ "If only has 3 arguments"


type' :: Expr -> STable  -> Expr 
type' (Var "type")   _ =  EString "type"
type' (Var "ltype")  _ =  EString "type"
type' (Var "if")     _ =  EString "any"
type' (Var "cvalue") _ =  EString "any"
type' e              st = case validate (E e) st of
    Right e         -> EString  . show $ e
    Left _          -> error "Impossible case"

fromRight (Right a) = a     


type'' :: StateT STable (Either String) Expr
type'' = do
    st   <- getTable  <$> get
    args <- getLazyArgList "type" 
    case args of
        [arg1'] -> do 
            st <- get
            let lt = ValidT.ValidT.lType 
            arg1 <- cValue <$> queryVal' arg1' 
            case arg1 of
                (E e) -> (EString . show . lt <$> queryVal' e) `mplus` return (type' e st)
                _     -> lift . Left $ "The expression does not has a type!"
            
        _      -> lift . Left $ "If only has 1 argument"

lType :: Expr -> StateT STable Maybe LipsT
lType = getExpLType

lType' :: StateT STable (Either String) Expr
lType' = do
    st   <- getTable  <$> get
    args <- getLazyArgList "ltype" 
    case args of
        [arg1'] -> do 
            st <- get
            let lt = ValidT.ValidT.lType 
            arg1 <- cValue <$> queryVal' arg1' 
            case arg1 of
                (E e) -> (EString . show . lt <$> queryVal' e) `mplus` lift (Left $ "The expression: " ++ regenerateExpr e ++ " does not have an ltype!")
                s     -> lift . Left $ "The expression: " ++ regenerateS s ++ " does not has an ltype!"
            
        _      -> lift . Left $ "If only has 1 argument"
        

cvalue :: Expr -> StateT STable Maybe Expr
cvalue = fmap (EString .  regenerateS) . getExpCValue


cvalue' :: StateT STable (Either String) Expr
cvalue' = do
    st   <- getTable  <$> get
    args <- getLazyArgList "cvalue" 
    case args of
        [arg1'] -> do 
            st <- get
            let lt = ValidT.ValidT.lType 
            arg1 <- cValue <$> queryVal' arg1' 
            case arg1 of
                (E e) -> (EString . regenerateS  . ValidT.ValidT.cValue  <$> queryVal' e) `mplus` lift (Left $ "The expression: " ++ regenerateExpr e ++ " does not have a cvalue!")
                s     -> lift . Left $ "The expression: " ++ regenerateS s ++ " does not has a cvalue!"
            
        _      -> lift . Left $ "If only has 1 argument"

reset :: Monad m => StateT STable m Expr 
reset = put iST >> return (mkBC True)

{-# NOINLINE irandom #-}
-- | Random number generator
irandom :: Int -> IO (Either String Int)
irandom n
    | n > 0 = Right <$> randomRIO (0, n - 1)
    | otherwise = return $ Left "The upper limit must be at least zero"



irandom' :: StateT STable (Either String) Expr
irandom' = do
    st   <- getTable  <$> get
    args <-  getArgList' "irandom" 
    case args of
        [arg1] ->  evalArithm True arg1 >>= fmap mkIC . lift . unsafePerformIO . irandom  
        _      -> lift . Left $ "If only has 1 argument"

-- | Fibonacci calculator
fibo :: Int -> Int
fibo n = fib !! n
    where
        fib = 0 : 1 : [a + b | (a, b) <- zip fib (tail fib)]

fibo' :: StateT STable (Either String) Expr
fibo' = do
    args <-  getArgList' "fibo" 
    case args of
        [arg1] -> mkIC . fibo <$> evalArithm True arg1
        _      -> lift . Left $ "If only has 1 argument"

gcd' :: StateT STable (Either String) Expr
gcd' = do 
    args <- getArgList' "gcd"   >>= traverse (evalArithm True) 
    case args of 
        [arg1,arg2] -> return . mkIC $ gcd arg1 arg2
        _           -> lift . Left $ "If only has 2 arguments"

-- | Milliseconds elapsed since January 1, 1970 at midnight UTC time 
now :: () -> Int
{-# NOINLINE now #-}
now () = unsafePerformIO $ round <$> getPOSIXTime 
    where
        aux = [round <$> getPOSIXTime]
        a2 = sequence aux 
        a3 = unsafePerformIO a2 
        a4 = head a3

-- | Logarithm in base two of an integer, returns the truncated result without decimals.
logB2 :: Int -> Int
logB2 = floor . logBase 2.0 . fromIntegral



logB2' :: StateT STable (Either String) Expr
logB2' = do
    args <-  getArgList' "logB2" 
    case args of
        [arg1] -> mkIC . logB2 <$> evalArithm True arg1
        _      -> lift . Left $ "If only has 1 argument"


-- | Converting a number from decimal to binary
fromDecimal :: Int -> [Int]
fromDecimal 0 = [0]
fromDecimal n = res : fromDecimal next
    where
        (next,res) = n `divMod` 2
{-
fromDecimal 0 = [0]
fromDecimal n = if (mod n 2 == 0) then 0:fromDecimal (div n 2) else 1:fromDecimal (div n 2)
-}


toBinary :: Int -> Int
toBinary n =negate $  foldl ((+).(*10)) 0 (reverse $ fromDecimal $ abs n)

toBinary' :: StateT STable (Either String) Expr
toBinary' = do
    args <-  getArgList' "toBinary" 
    case args of
        [arg1] -> mkIC . toBinary <$> evalArithm True arg1
        _      -> lift . Left $ "If only has 1 argument"