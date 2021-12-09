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
data S = A Action | E Expr

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

-- | Action tree
data Action
    = Declaration LipsT String Expr
    | Assignment  String Expr
    | SeqA        Action S

-- | Constant definition
data Constant n
    = BConstant Bool
    | NumConstant n


-- | Lips types
data LipsT = LInt | LBool | LLazy LipsT deriving (Eq)

-- | Helper type to pretty print things
newtype TS = T (Tree String)

------------------------------
-- String related functions
------------------------------

instance Show LipsT where
    show LInt      = "int"
    show LBool     = "bool"
    show (LLazy t) = "lazy " ++ show t

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

-- | Transforms the implicit tree into a Tree type (so we can prettyPrint it using the library!)
sToTree :: S -> Tree String
sToTree (E expr)   = exprToTree expr
sToTree (A action) = actionToTree action

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
exprToTree (Negate e)  = Node {rootLabel= "UMinus" , subForest=[exprToTree e]}
exprToTree (Pos    e)  = Node {rootLabel= "UPlusr" , subForest=[exprToTree e]}
exprToTree (Not    e)  = Node {rootLabel= "!" , subForest=[exprToTree e]}
exprToTree (Plus a b)  = Node {rootLabel= "+"     , subForest=[exprToTree a, exprToTree b]}
exprToTree (Minus a b) = Node {rootLabel= "-"     , subForest=[exprToTree a, exprToTree b]}
exprToTree (Times t t')= Node {rootLabel= "*"     , subForest=[exprToTree t, exprToTree t' ]}
exprToTree (Mod t t')  = Node {rootLabel= "%"     , subForest=[exprToTree t, exprToTree t' ]}
exprToTree (Pow t t')  = Node {rootLabel= "^"     , subForest=[exprToTree t, exprToTree t' ]}
exprToTree (Var v)     = Node {rootLabel= "Var " ++ v  , subForest=[]}
exprToTree (EQ p q)    = Node {rootLabel= "="     , subForest=[exprToTree p, exprToTree q]}
exprToTree (NEQ p q)   = Node {rootLabel= "<>"    , subForest=[exprToTree p, exprToTree q]}
exprToTree (LT p q)    = Node {rootLabel= "<"     , subForest=[exprToTree p, exprToTree q]}
exprToTree (GT p q)    = Node {rootLabel= ">"     , subForest=[exprToTree p, exprToTree q]}
exprToTree (LE p q)    = Node {rootLabel= "<="    , subForest=[exprToTree p, exprToTree q]}
exprToTree (GE p q)    = Node {rootLabel= ">="    , subForest=[exprToTree p, exprToTree q]}
exprToTree (Or p q)    = Node {rootLabel= "||"    , subForest=[exprToTree p, exprToTree q]}
exprToTree (And p q)   = Node {rootLabel= "&&"    , subForest=[exprToTree p, exprToTree q]}
exprToTree (Lazy e)    = Node {rootLabel= "LazyE", subForest=[exprToTree e]}
exprToTree (SeqE e s)  = Node {rootLabel= "Sequence", subForest=[exprToTree e, sToTree s]}
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
showAST = show
