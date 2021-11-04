{
module Grammar.Grammar (toAST, sToTree, prettyPrintS, parse' ) where

import Data.Char
import Lexer.Lexer (Token(..), manyToken)
import Data.Tree  
import Data.Tree.Pretty (drawVerticalTree)
}

%name toAST
%tokentype { Token }
%error {parseError}

%token
    true  { TkTrue      }
    false { TkFalse     }
    mod   { TkMod       }
    lazy  { TkLazy      }
    int   { TkInt       }
    bool  { TkBool      }
    while { TkWhile     }
    if    { TkIf        }
    type  { TkType      }
    '`'   { TkQuote     }
    ','   { TkComma     }
    ';'   { TkSemicolon }
    TkId  { TkId  $$    }
    TkNum { TkNum $$    }
    '^'   { TkPower     }
    '*'   { TkMult      }
    '+'   { TkPlus      }
    '-'   { TkMinus     }
    '<='  { TkLE        }
    '>='  { TkGE        }
    '<'   { TkLT        }
    '>'   { TkGT        }
    '='   { TkEQ        }
    '<>'  { TkNE        }
    '!'   { TkNot       }
    '||'  { TkOr        }
    '&&'  { TkAnd       }
    ':='  { TkAssign    }
    '('   { TkOpenPar   }
    ')'   { TkClosePar  }
    '{'   { TkOpenBrace }
    '}'   { TkCloseBrace}

%right '||' 
%right '&&' 
%nonassoc '=' '<>'
%nonassoc '>' '<' '>=' '<='
%left '+' '-'
%left '*' '%'
%left NEG PLS
%right '^'

%%


S 
    : Expr                   { Expr   $1 }
    


Expr
    : '-' Unary %prec NEG       { Negate   $2 }
    | '+' Unary %prec PLS       { Pos      $2 }
    | Expr '=' Expr             { EQ'   $1 $3 }
    | Expr '<>' Expr            { NEQ'  $1 $3 }
    | Expr '+' Expr             { Plus  $1 $3 }
    | Expr '-' Expr             { Minus $1 $3 }
    | Expr '*' Expr             { Times $1 $3 }
    | Expr '^' Expr             { Pow   $1 $3 }
    | Expr '<' Expr             { LT'   $1 $3 }
    | Expr '>' Expr             { GT'   $1 $3 }
    | Expr '<=' Expr            { LE'   $1 $3 }
    | Expr '>=' Expr            { GE'   $1 $3 }
    | Expr '||' Expr            { Or    $1 $3 }
    | Expr '&&' Expr            { And   $1 $3 }
    | TkNum                     { Num   $1    }
    | TkId                      { Var   $1    }
    | '(' Expr ')'              { Brack $2    }
    | FApp                      {       $1    }
    | Constant                  {       $1    }

Constant
    : true                      { BOOL True    }
    | false                     { BOOL False   }

FApp
    : TkId '(' Args ')'         { FApp  $1 $3 }

Args
    : {- empty -}               { [] }
    | NEArgs                    { reverse $1 }

NEArgs 
    : Expr                      { $1 : []  }
    | NEArgs  ',' Expr          { $3 : $1 }

Unary
    : TkNum                     { Num   $1 }
    | FApp                      {       $1 }
    | TkId                      { Var   $1 }
    | '(' Expr ')'              { Brack $2 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data S 
    = Expr Expr
        deriving Show

data Expr
    = Brack  Expr
    | Negate Expr
    | Pos    Expr 
    | Plus   Expr Expr
    | Minus  Expr Expr
    | Mod    Expr Expr 
    | Times  Expr Expr 
    | Pow    Expr Expr
    | EQ'    Expr Expr 
    | NEQ'   Expr Expr
    | Or     Expr Expr
    | And    Expr Expr
    | LT'    Expr Expr
    | GT'    Expr Expr
    | LE'    Expr Expr
    | GE'    Expr Expr
    | Num    Int
    | Var    String
    | BOOL   Bool
    | FApp   String [Expr]
        deriving Show



sToTree :: S -> Tree String
sToTree (Expr e) = exprToTree e 

exprToTree :: Expr -> Tree String
exprToTree (Negate e)  = Node {rootLabel= "-Expr", subForest=[exprToTree e]}
exprToTree (Pos    e)  = Node {rootLabel= "+Expr", subForest=[exprToTree e]}
exprToTree (Plus a b)  = Node {rootLabel= "+", subForest=[exprToTree a, exprToTree b]}
exprToTree (Minus a b) = Node {rootLabel= "-", subForest=[exprToTree a, exprToTree b]}
exprToTree (Times t t')= Node {rootLabel= "*", subForest=[exprToTree t, exprToTree t' ]}
exprToTree (Mod t t')  = Node {rootLabel= "%", subForest=[exprToTree t, exprToTree t' ]}
exprToTree (Pow t t')  = Node {rootLabel= "^", subForest=[exprToTree t, exprToTree t' ]}
exprToTree (Num n)     = Node {rootLabel= show n, subForest=[]}
exprToTree (Var v)     = Node {rootLabel= show v, subForest=[]}
exprToTree (Brack e)   = Node {rootLabel= "(Expr)", subForest=[exprToTree e]}
exprToTree (BOOL p)    = Node {rootLabel=show p, subForest=[]}
exprToTree (EQ' p q)   = Node {rootLabel="=", subForest=[exprToTree p, exprToTree q]}
exprToTree (NEQ' p q)  = Node {rootLabel="<>", subForest=[exprToTree p, exprToTree q]}
exprToTree (LT' p q)   = Node {rootLabel="<", subForest=[exprToTree p, exprToTree q]}
exprToTree (GT' p q)   = Node {rootLabel=">", subForest=[exprToTree p, exprToTree q]}
exprToTree (LE' p q)   = Node {rootLabel="<=", subForest=[exprToTree p, exprToTree q]}
exprToTree (GE' p q)   = Node {rootLabel=">=", subForest=[exprToTree p, exprToTree q]}
exprToTree (Or p q)    = Node {rootLabel="||", subForest=[exprToTree p, exprToTree q]}
exprToTree (And p q)   = Node {rootLabel="&&", subForest=[exprToTree p, exprToTree q]}
exprToTree (FApp name args) = Node {rootLabel="f(..)", subForest= map exprToTree args }

toPrettyS :: S -> String
toPrettyS  = drawVerticalTree . sToTree 

prettyPrintS :: S -> IO ()
prettyPrintS = putStrLn . toPrettyS


parse :: String -> Either [(String,Int)] String
parse = fmap (toPrettyS . toAST)  . manyToken

parse' :: String -> IO ()
parse' s = do
    let ast = parse s
    case ast of
        Right res -> putStrLn res
        Left _    -> putStrLn "Error"

}

