module AST.AST where
import Data.Char
import Lexer.Lexer (Token(..), manyToken)
import Data.Tree  
import Data.Tree.Pretty (drawVerticalTree)
import Prelude hiding (EQ,LT,GT)

data S = A Action | E Expr

data Expr
    = Negate Expr
    | Pos    Expr 
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


data Action
    = Declaration LipsT String Expr
    | Assignment  String Expr

data Constant n
    = BConstant Bool
    | NumConstant n



data LipsT = LInt | LBool | LLazy LipsT 

instance Show LipsT where
    show LInt      = "int"
    show LBool     = "bool"
    show (LLazy t) = "lazy " ++ show t 


sToTree :: S -> Tree String
sToTree (E expr)   = exprToTree expr
sToTree (A action) = actionToTree action


actionToTree :: Action -> Tree String
actionToTree (Declaration t v e) = Node {rootLabel= ":=", subForest=[n',exprToTree e]}
    where
        n' = Node {rootLabel= show t ++ " " ++ v, subForest=[]  }
actionToTree (Assignment v e) = Node {rootLabel= ":=", subForest=[n',exprToTree e]}
    where
        n' = Node {rootLabel= v, subForest=[]}

exprToTree :: Expr -> Tree String
exprToTree (Negate e)  = Node {rootLabel= "-Expr" , subForest=[exprToTree e]}
exprToTree (Pos    e)  = Node {rootLabel= "+Expr" , subForest=[exprToTree e]}
exprToTree (Plus a b)  = Node {rootLabel= "+"     , subForest=[exprToTree a, exprToTree b]}
exprToTree (Minus a b) = Node {rootLabel= "-"     , subForest=[exprToTree a, exprToTree b]}
exprToTree (Times t t')= Node {rootLabel= "*"     , subForest=[exprToTree t, exprToTree t' ]}
exprToTree (Mod t t')  = Node {rootLabel= "%"     , subForest=[exprToTree t, exprToTree t' ]}
exprToTree (Pow t t')  = Node {rootLabel= "^"     , subForest=[exprToTree t, exprToTree t' ]}
exprToTree (Var v)     = Node {rootLabel= show v  , subForest=[]}
exprToTree (EQ p q)    = Node {rootLabel= "="     , subForest=[exprToTree p, exprToTree q]}
exprToTree (NEQ p q)   = Node {rootLabel= "<>"    , subForest=[exprToTree p, exprToTree q]}
exprToTree (LT p q)    = Node {rootLabel= "<"     , subForest=[exprToTree p, exprToTree q]}
exprToTree (GT p q)    = Node {rootLabel= ">"     , subForest=[exprToTree p, exprToTree q]}
exprToTree (LE p q)    = Node {rootLabel= "<="    , subForest=[exprToTree p, exprToTree q]}
exprToTree (GE p q)    = Node {rootLabel= ">="    , subForest=[exprToTree p, exprToTree q]}
exprToTree (Or p q)    = Node {rootLabel= "||"    , subForest=[exprToTree p, exprToTree q]}
exprToTree (And p q)   = Node {rootLabel= "&&"    , subForest=[exprToTree p, exprToTree q]}
exprToTree (Lazy e)    = Node {rootLabel= "'Expr'", subForest=[exprToTree e]}
exprToTree (C c)       = case c of
        BConstant b   -> Node {rootLabel= show b  , subForest=[]}
        NumConstant n -> Node {rootLabel= show n  , subForest=[]}
exprToTree (FApp name args) = Node {rootLabel="f(..)", subForest= map exprToTree args }


toPrettyS :: S -> String
toPrettyS  = drawVerticalTree . sToTree 

prettyPrintS :: S -> IO ()
prettyPrintS = putStrLn . toPrettyS
