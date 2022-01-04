module AST.AST where

{- |
Module      : AST
Description : Provides the definition of the AST.
Maintainer  : 15-11139@usb.ve, 16-10400@usb.ve 17-10538@usb.ve
Stability   : experimental
Portability : POSIX

-}


import           Data.List        (intercalate)
import           Data.Tree        (Tree (..))
import           Data.Tree.Pretty (drawVerticalTree)
import           Lexer.Lexer      (Token (..), manyToken)
import           Prelude          hiding (EQ, GT, LT)

------------------------------
-- Types
------------------------------

-- | AST tree
data S = A Action | E Expr | Seq S S  deriving (Eq,Ord)

-- | Expression tree
data Expr
    = Negate Expr
    | Pos    Expr
    | Not    Expr
    | Plus   Expr Expr
    | Minus  Expr Expr
    | Mod    Expr Expr
    | Times  Expr Expr
    | Pow    Expr Expr
    | EQ     Expr Expr
    | NEQ    Expr Expr
    | LT     Expr Expr
    | GT     Expr Expr
    | LE     Expr Expr
    | GE     Expr Expr
    | Or     Expr Expr
    | And    Expr Expr
    | C      (Constant Int)
    | Var    String
    | FApp   String [Expr]
    | Lazy   Expr
    | SeqE   Expr S
    | EString String
    deriving (Eq, Ord)

-- | Action tree
data Action
    = Declaration LipsT String Expr
    | Assignment  String Expr
    | SeqA        Action S
    deriving (Eq,Ord)

-- | Constant definition
data Constant n
    = BConstant Bool
    | NumConstant n
    deriving (Eq,Ord)

-- | Lips types
data LipsT = LInt | LBool | LString | LFloat | LLazy LipsT | Type | Void  | Any | ConstantT LipsT | Fun [LipsT] LipsT deriving (Eq,Ord)

-- si el hijo tiene menor precedencia, agrupelo con parentesis.
getPrec :: Expr -> Int
getPrec (Lazy _)    = 10
getPrec (Var _)     = 10
getPrec (FApp _ _)  = 10
getPrec (C _)       = 10 
getPrec (EString _) = 10
getPrec (Pow _ _)   = 9
getPrec (Negate _)  = 8
getPrec (Not _)     = 8
getPrec (Pos _)     = 8
getPrec (Times _ _) = 7
getPrec (Mod _ _)   = 7
getPrec (Plus _ _)  = 6
getPrec (Minus _ _) = 6
getPrec (LT _ _)    = 5
getPrec (GT _ _)    = 5
getPrec (LE _ _)    = 5
getPrec (GE _ _)    = 5
getPrec (EQ _ _)    = 4
getPrec (NEQ _ _)   = 4
getPrec (And _ _)   = 3
getPrec (Or _ _)    = 2
getPrec (SeqE _ _)  = 1 

regenerateChild :: Expr -> Expr -> String
regenerateChild father child 
    | getPrec father > getPrec child = "(" ++ regenerateExpr child ++ ")"
    | otherwise                      = regenerateExpr child

regenerateExpr :: Expr -> String
regenerateExpr n@(Negate e)                 = '-' : regenerateChild n e
regenerateExpr (Pos e)                      = regenerateExpr e
regenerateExpr n@(Not e)                    = '!' : regenerateChild n e
regenerateExpr p@(Plus a b)                 = regenerateChild p a ++ " + " ++ regenerateChild p b
regenerateExpr m@(Minus a b)                = regenerateChild m a ++ " - " ++ regenerateChild m b
regenerateExpr t@(Times a b)                = regenerateChild t a ++ " * " ++ regenerateChild t b
regenerateExpr m@(Mod a b)                  = regenerateChild m a ++ " % " ++ regenerateChild m b
regenerateExpr p@(Pow a b)                  = regenerateChild p a ++ " ^ " ++ regenerateChild p b
regenerateExpr c@(LT a b)                   = regenerateChild c a ++ " < " ++ regenerateChild c b
regenerateExpr c@(GT a b)                   = regenerateChild c a ++ " > " ++ regenerateChild c b
regenerateExpr c@(LE a b)                   = regenerateChild c a ++ " <= " ++ regenerateChild c b
regenerateExpr c@(GE a b)                   = regenerateChild c a ++ " >= " ++ regenerateChild c b
regenerateExpr c@(EQ a b)                   = regenerateChild c a ++ " = " ++ regenerateChild c b
regenerateExpr c@(NEQ a b)                  = regenerateChild c a ++ " <> " ++ regenerateChild c b 
regenerateExpr c@(Or a b)                   = regenerateChild c a ++ " || " ++ regenerateChild c b 
regenerateExpr c@(And a b)                  = regenerateChild c a ++ " && " ++ regenerateChild c b
regenerateExpr c@(Lazy a)                   = "'" ++ regenerateExpr a ++ "'"
regenerateExpr (FApp fName args)            = fName ++ "(" ++ intercalate "," (map regenerateExpr args) ++ ")"
regenerateExpr (Var vName)                  = vName
regenerateExpr (C (NumConstant n))          = show n
regenerateExpr (C (BConstant b))            = show b
regenerateExpr (EString s)                  = show s
regenerateExpr c@(SeqE a b)                 = undefined


regenerateAction :: Action -> String
regenerateAction (Declaration t vName e) = show t ++ " " ++ vName ++ " := " ++ regenerateExpr e
regenerateAction (Assignment  vName e)   = vName ++ " := " ++ regenerateExpr e
regenerateAction _                       = undefined 

regenerateS :: S -> String
regenerateS (A a) = regenerateAction a ++ ";"
regenerateS (E e) = regenerateExpr e ++ ";"
regenerateS (Seq a b) = regenerateS a ++ "; " ++ regenerateS b ++ ";"

-- | Helper type to pretty print things
newtype TS = T (Tree String)

------------------------------
-- String related functions
------------------------------

instance Show LipsT where
    show LInt      = "int"
    show LBool     = "bool"
    show (LLazy t) = "lazy " ++ show t
    show Type      = "type"
    show Void      = "()"
    show Any       = "Any"
    show (ConstantT t) = "Constant " ++ show t
    show (Fun ts t) = "fun(" ++ intercalate "," [show t' | t' <- ts] ++ ") -> " ++ show t

-- Since we have already a tree of strings that represents
-- we might as well defined the desired showAST as an pre-order
-- traversal of the tree.
instance Show TS where
    show (T Node{rootLabel=r , subForest=[]})  = r
    show (T Node{rootLabel=r , subForest=sub}) = r ++ "(" ++ intercalate "," (fmap (show . T) sub) ++ ")"

instance Show Action where
    show action = show $ T $ actionToTree action

instance Show Expr where
    show expr = show $ T $ exprToTree expr

instance Show S where
    show (A action) = show action
    show (E expr)   = show expr
    show (Seq a b)  = show a ++ "; " ++ show b

-- | Transforms the implicit tree into a Tree type (so we can prettyPrint it using the library!)
sToTree :: S -> Tree String
sToTree (E expr)   = exprToTree expr
sToTree (A action) = actionToTree action
sToTree (Seq a b)  = Node {rootLabel= ";", subForest=[sToTree a,sToTree b]}


-- | Transforms the Action tree into a Tree type
actionToTree :: Action -> Tree String
actionToTree (Declaration t v e) = Node {rootLabel= ":=", subForest=[n',exprToTree e]}
    where
        n' = Node {rootLabel= show t ++ " " ++ v, subForest=[]  }
actionToTree (Assignment v e) = Node {rootLabel= ":=", subForest=[n',exprToTree e]}
    where
        n' = Node {rootLabel= v, subForest=[]}
actionToTree (SeqA a s) = Node {rootLabel= "Sequence", subForest=[actionToTree a,sToTree s]}


-- | Transforms the Expr tree into a Tree type
exprToTree :: Expr -> Tree String
exprToTree (Negate e)  = Node {rootLabel= "UMinus"     , subForest=[exprToTree e]}
exprToTree (Pos    e)  = Node {rootLabel= "UPlus"      , subForest=[exprToTree e]}
exprToTree (Not    e)  = Node {rootLabel= "!"          , subForest=[exprToTree e]}
exprToTree (Plus a b)  = Node {rootLabel= "+"          , subForest=[exprToTree a, exprToTree b]}
exprToTree (Minus a b) = Node {rootLabel= "-"          , subForest=[exprToTree a, exprToTree b]}
exprToTree (Times t t')= Node {rootLabel= "*"          , subForest=[exprToTree t, exprToTree t' ]}
exprToTree (Mod t t')  = Node {rootLabel= "%"          , subForest=[exprToTree t, exprToTree t' ]}
exprToTree (Pow t t')  = Node {rootLabel= "^"          , subForest=[exprToTree t, exprToTree t' ]}
exprToTree (Var v)     = Node {rootLabel= "Var " ++ v  , subForest=[]}
exprToTree (EQ p q)    = Node {rootLabel= "="          , subForest=[exprToTree p, exprToTree q]}
exprToTree (NEQ p q)   = Node {rootLabel= "<>"         , subForest=[exprToTree p, exprToTree q]}
exprToTree (LT p q)    = Node {rootLabel= "<"          , subForest=[exprToTree p, exprToTree q]}
exprToTree (GT p q)    = Node {rootLabel= ">"          , subForest=[exprToTree p, exprToTree q]}
exprToTree (LE p q)    = Node {rootLabel= "<="         , subForest=[exprToTree p, exprToTree q]}
exprToTree (GE p q)    = Node {rootLabel= ">="         , subForest=[exprToTree p, exprToTree q]}
exprToTree (Or p q)    = Node {rootLabel= "||"         , subForest=[exprToTree p, exprToTree q]}
exprToTree (And p q)   = Node {rootLabel= "&&"         , subForest=[exprToTree p, exprToTree q]}
exprToTree (Lazy e)    = Node {rootLabel= "LazyE"      , subForest=[exprToTree e]}
exprToTree (SeqE e s)  = Node {rootLabel= "Sequence"   , subForest=[exprToTree e, sToTree s]}
exprToTree (EString s) = Node {rootLabel= s            , subForest=[]}
exprToTree (C c)       = case c of
        BConstant b   -> Node {rootLabel= show b  , subForest=[]}
        NumConstant n -> Node {rootLabel= show n  , subForest=[]}
exprToTree (FApp name args) = Node {rootLabel="FApp", subForest= name' : map exprToTree args }
    where
        name' = Node {rootLabel= name , subForest=[]}

-- | Pretty printing function!
toPrettyS :: S -> String
toPrettyS  = drawVerticalTree . sToTree

-- | Pretty printing function! but actually prints it.
prettyPrintS :: S -> IO ()
prettyPrintS = putStrLn . toPrettyS


-- | showAST is just the show instance, which is the pre-order traversal of the tree.
showAST :: S -> String
showAST = (++";")  . show

------------------------------
-- Tobe functions
------------------------------

-- === AST ===
--data S = A Action | E Expr
sisAction :: S -> Bool
sisAction (A _) = True
sisAction _ = False

sTakeExpr :: S -> Expr
sTakeExpr (E x) = x

sTakeAction :: S -> Action
sTakeAction (A x) = x

-- === Actions ===

aisDeclaration :: Action -> Bool
aisDeclaration (Declaration _ _ _) = True
aisDeclaration _ = False

takeDeclaration :: Action -> (LipsT, String, Expr)
takeDeclaration (Declaration v1 v2 v3) = (v1,v2,v3)

aisAssignment :: Action -> Bool
aisAssignment (Assignment _ _) = True
aisAssignment _ = False

takeAssignment :: Action -> (String, Expr)
takeAssignment (Assignment v1 v2) = (v1,v2)

-- === Expresions ===

-- Base cases
exprIsC :: Expr -> Bool
exprIsC (C _) = True
exprIsC _ = False

takeTypeC :: Expr -> LipsT
takeTypeC (C (BConstant x)) = LBool
takeTypeC (C (NumConstant x)) = LInt

exprIsVar :: Expr -> Bool
exprIsVar (Var _) = True
exprIsVar _ = False

takeVar :: Expr -> String
takeVar ( Var x ) = x

exprIsLazy :: Expr -> Bool
exprIsLazy (Lazy _) = True
exprIsLazy _ = False

takeLazy :: Expr -> Expr
takeLazy (Lazy x) = x

-- Each symbol have only one type associated
-- ( This can be generalized with a list )

-- Unary expression
exprIsUnary:: Expr -> Bool
exprIsUnary(Pos _) = True
exprIsUnary(Negate _) = True
exprIsUnary (Not _) = True
exprIsUnary _ = False

getTypeUnaryExpr :: Expr -> LipsT
getTypeUnaryExpr(Pos _) = LInt
getTypeUnaryExpr(Negate _) = LInt
getTypeUnaryExpr (Not _) = LBool

exprUnaryTake :: Expr -> Expr
exprUnaryTake (Pos x) = x
exprUnaryTake (Not x) = x
exprUnaryTake (Negate x) = x

-- Binary expresion
exprIsBinary :: Expr -> Bool
exprIsBinary ( Plus _ _) = True
exprIsBinary ( Minus _ _) = True
exprIsBinary ( Mod _ _) = True
exprIsBinary ( Times _ _) = True
exprIsBinary ( Pow _ _) = True
exprIsBinary ( EQ _ _) = True
exprIsBinary ( NEQ _ _) = True
exprIsBinary ( LT _ _) = True
exprIsBinary ( GT _ _) = True
exprIsBinary ( LE _ _) = True
exprIsBinary ( GE _ _) = True
exprIsBinary ( Or _ _) = True
exprIsBinary ( And _ _) = True
exprIsBinary ( _ ) = False 

getTypeBinaryExpr :: Expr -> LipsT
getTypeBinaryExpr ( Plus _ _) = LInt
getTypeBinaryExpr ( Minus _ _) = LInt
getTypeBinaryExpr ( Mod _ _) = LInt
getTypeBinaryExpr ( Times _ _) = LInt
getTypeBinaryExpr ( Pow _ _) = LInt
getTypeBinaryExpr ( EQ _ _) = LBool
getTypeBinaryExpr ( NEQ _ _) = LBool
getTypeBinaryExpr ( LT _ _) = LBool
getTypeBinaryExpr ( GT _ _) = LBool
getTypeBinaryExpr ( LE _ _) = LBool
getTypeBinaryExpr ( GE _ _) = LBool
getTypeBinaryExpr ( Or _ _) = LBool
getTypeBinaryExpr ( And _ _) = LBool

takeFromBinaryExpr :: Expr -> (Expr,Expr)
takeFromBinaryExpr ( Plus ex1 ex2 ) = (ex1,ex2)
takeFromBinaryExpr ( Minus ex1 ex2 ) = (ex1,ex2)
takeFromBinaryExpr ( Mod ex1 ex2 ) = (ex1,ex2)
takeFromBinaryExpr ( Times ex1 ex2 ) = (ex1,ex2)
takeFromBinaryExpr ( Pow ex1 ex2 ) = (ex1,ex2)
takeFromBinaryExpr ( EQ ex1 ex2 ) = (ex1,ex2)
takeFromBinaryExpr ( NEQ ex1 ex2 ) = (ex1,ex2)
takeFromBinaryExpr ( LT ex1 ex2 ) = (ex1,ex2)
takeFromBinaryExpr ( GT ex1 ex2 ) = (ex1,ex2)
takeFromBinaryExpr ( LE ex1 ex2 ) = (ex1,ex2)
takeFromBinaryExpr ( GE ex1 ex2 ) = (ex1,ex2)
takeFromBinaryExpr ( Or ex1 ex2 ) = (ex1,ex2)
takeFromBinaryExpr ( And ex1 ex2 ) = (ex1,ex2)

-- Secuencia de expresiones
exprIsSeqE :: Expr -> Bool
exprIsSeqE (SeqE _ _) = True
exprIsSeqE _ = False

takeSeqE :: Expr -> (Expr,S)
takeSeqE (SeqE ex ast) = (ex,ast)

-- FApp
exprIsFApp :: Expr -> Bool
exprIsFApp (FApp _ _) = True
exprIsFApp _ = False

takeFApp :: Expr -> (String,[Expr])
takeFApp (FApp name lista) = (name,lista)
