{- |
Module      : Tokens
Description : Provides necessary tools for lexing.
Maintainer  : 15-11139@usb.ve, 16-10400@usb.ve 17-10538@usb.ve
Stability   : experimental
Portability : POSIX
-}

module Parser.Tokens where
import Lexer.Lexer

isTkId :: Token -> Bool
isTkId (TkId _) = True
isTkId _ = False

isTkNum :: Token -> Bool
isTkNum (TkNum _) = True
isTkNum _ = False

getTkNum :: Token -> Int 
getTkNum ( TkNum x ) = x

getTkId :: Token -> String
getTkId ( TkId x ) = x

