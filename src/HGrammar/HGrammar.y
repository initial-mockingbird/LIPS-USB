{
module HGrammar.HGrammar (toAST, sToTree, prettyPrintS, parse ) where

import Data.Char
import Lexer.Lexer (Token(..), manyToken, PrettyToken(..))
import Data.Tree  
import Data.Tree.Pretty (drawVerticalTree)
import AST.AST
import Data.List (intercalate)
import Prelude hiding (EQ,LT,GT)
}



%monad { Either (String,Int)}
%name toAST
%tokentype { Token }
%error {parseError}

%token
    true  { TkTrue      }
    false { TkFalse     }
    '%'   { TkMod       }
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
%nonassoc NEG PLS '!'
%right '^'

%%
 

S 
    : Expr                      { E   $1 }
    | Action                    { A   $1 }
    | Expr    ';' S             { E (SeqE $1 $3)}
    | Action  ';' S             { A (SeqA $1 $3)}
    | Action  ';' {-empty-}     { A $1 }
    | Expr    ';' {-empty-}     { E $1 }


Action 
    : Declaration            {$1}
    | Assignment             {$1}

Declaration
    : Type Assignment        { (\(Assignment s e) -> Declaration $1 s e )  $2 }

Type 
    : int         {LInt     }
    | bool        {LBool    }
    | lazy Type   {LLazy $2 }

Assignment 
    : TkId ':=' Expr {Assignment $1 $3}

Expr
    : '-' Expr %prec NEG        { Negate   $2 }
    | '+' Expr %prec PLS        { Pos      $2 }
    | '!' Expr                  { Not      $2 }
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
    | Expr '%'  Expr            { Mod   $1 $3 }
    | TkId                      { Var   $1    }
    | type                      { Var "type"  }
    | if                        { Var "if"    }
    | '(' Expr ')'              {       $2    }
    | '`' Expr '`'              { Lazy  $2    }
    | FApp                      {       $1    }
    | Constant                  {       $1    }


Constant
    : true                      { toBoolC True    }
    | false                     { toBoolC False   }
    | TkNum                     { toNumC  $1      }

FApp
    : TkId   '(' Args ')'       { FApp  $1 $3 }
    | type   '(' Args ')'       { FApp  "type" $3}
    | if     '(' Args ')'       { FApp  "if" $3}



Args
    : {- empty -}               { [] }
    | NEArgs                    { reverse $1 }

NEArgs 
    : Expr                      { $1 : []  }
    | NEArgs  ',' Expr          { $3 : $1 }
    | NEArgs  ',' {- empty -}   { % Left ("Parse error on argument list. ", 0)}



Unary
    : TkNum                     { toNumC  $1 }
    | FApp                      {         $1 }
    | TkId                      { Var     $1 }
    | '(' Expr ')'              {         $2 }

{
parseError :: [Token] -> Either (String,Int) a
parseError fault = Left $ ("Parse error at expression: " ++ intercalate " " (map (show . PT) fault), length fault)
parseError []    = Left ("Lambda does not belong to the language",-1)

toNumC :: Int -> Expr
toNumC  = C . NumConstant

toBoolC :: Bool -> Expr
toBoolC  = C . BConstant


parseLine' :: Int -> [Token] -> Either (String,Int) S
parseLine' n tks = case toAST tks of
    Left (errMsg,remanent) -> Left (errMsg,n-remanent)
    s                      -> s

parse :: String -> Either (String,Int) S
parse input = do 
    let 
        
        manyToken' :: String -> Either (String,Int) [Token]
        manyToken' x' = case manyToken x' of
            Left (e:_)  -> Left e
            (Right tks) -> Right tks 
        
        toAST' :: [Token] -> Either (String,Int) S 
        toAST' tks = case parseLine' (length tks) tks of
            Left (err,n) -> Left (err ++ " at token: " ++ show n,n)
            ast          -> ast
        
    tokens' <- manyToken' input
    toAST' tokens'
             


parse' :: String -> String
parse' x = case parse x of
    Left (errMsg,_) -> errMsg
    Right ast       -> toPrettyS ast
        

}

