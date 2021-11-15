{
module HGrammar.HGrammar (toAST, sToTree, prettyPrintS, parse' ) where

import Data.Char
import Lexer.Lexer (Token(..), manyToken)
import Data.Tree  
import Data.Tree.Pretty (drawVerticalTree)
import AST.AST
import Prelude hiding (EQ,LT,GT)
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
    : Expr                   { E   $1 }
    


Expr
    : '-' Unary %prec NEG       { Negate   $2 }
    | '+' Unary %prec PLS       { Pos      $2 }
    | Expr '=' Expr             { EQ    $1 $3 }
    | Expr '<>' Expr            { NEQ   $1 $3 }
    | Expr '+' Expr             { Plus  $1 $3 }
    | Expr '-' Expr             { Minus $1 $3 }
    | Expr '*' Expr             { Times $1 $3 }
    | Expr '^' Expr             { Pow   $1 $3 }
    | Expr '<' Expr             { LT    $1 $3 }
    | Expr '>' Expr             { GT    $1 $3 }
    | Expr '<=' Expr            { LE    $1 $3 }
    | Expr '>=' Expr            { GE    $1 $3 }
    | Expr '||' Expr            { Or    $1 $3 }
    | Expr '&&' Expr            { And   $1 $3 }
    | TkId                      { Var   $1    }
    | '(' Expr ')'              {       $2    }
    | FApp                      {       $1    }
    | Constant                  {       $1    }

Constant
    : true                      { toBoolC True    }
    | false                     { toBoolC False   }
    | TkNum                     { toNumC  $1      }

FApp
    : TkId '(' Args ')'         { FApp  $1 $3 }

Args
    : {- empty -}               { [] }
    | NEArgs                    { reverse $1 }

NEArgs 
    : Expr                      { $1 : []  }
    | NEArgs  ',' Expr          { $3 : $1 }

Unary
    : TkNum                     { toNumC  $1 }
    | FApp                      {         $1 }
    | TkId                      { Var     $1 }
    | '(' Expr ')'              {         $2 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

toNumC :: Int -> Expr
toNumC  = C . NumConstant

toBoolC :: Bool -> Expr
toBoolC  = C . BConstant

parse :: String -> Either [(String,Int)] String
parse = fmap (toPrettyS . toAST)  . manyToken

parse' :: String -> IO ()
parse' s = do
    let ast = parse s
    case ast of
        Right res -> putStrLn res
        Left _    -> putStrLn "Error"

}

