module Main where

import Lib
--import REPL.REPL ( initialST, repl )
import REPL.REPL ( initialST, repl )
import System.IO


main :: IO ()
-- buffering doesnt behave well with the ghci/terminal, so we disable it.
main = hSetBuffering stdout NoBuffering >> repl initialST
