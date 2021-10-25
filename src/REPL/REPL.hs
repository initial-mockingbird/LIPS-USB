{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{- |
Module      : REPL
Description : Module that has provides the REPL utility.
Maintainer  : 15-11139@usb.ve, 16-10400@usb.ve 17-10538@usb.ve
Stability   : experimental
Portability : POSIX

-}
module REPL.REPL where

import           Control.Monad                 (unless, when)
import           Control.Monad.State.Strict
import           Data.ByteString               (ByteString, pack, readFile)
import           Data.ByteString.Char8         (lines, unpack)
import           Data.Char                     (ord)
import           Data.Either                   hiding (choice)
import           Data.Foldable                 (traverse_)
import           Data.Functor                  (($>))
import           Data.Maybe                    (fromMaybe)
import qualified Data.Set                      as S
import           Debug.Trace
import           Lexer.Lexer
import           Prelude                       hiding (lex, lines, readFile)
import           System.Exit                   (exitSuccess)
import           System.IO                     hiding (readFile)
import           Text.Parsec.Char
import           Text.Parsec.Error             (errorMessages, messageString)
import           Text.ParserCombinators.Parsec hiding (get, token, tokens)
import           Text.Read                     (readMaybe)
import           Text.Read                     hiding (choice, get, lex, reset)

------------------------------
-- Types
------------------------------

-- | Holds the current state of the REPL.
data SessionState = ST 
    { errors      :: [REPLError]    -- ^ A list of all the errors so far.
    , loadedFiles :: S.Set FilePath -- ^ A Set containing the loaded Files, used in the future to detect loading loops.
    , loadingMode :: Bool           -- ^ Determines if the current state is originated from a @.load@ call
    , actualFile  :: Maybe FilePath -- ^ The file that was loaded by the @.load@ call
    , env         :: Bool           -- ^ We need an environment to hold the variables in the future
    } deriving Show


-- | Errors are formated as triples: <File name, Position, error msg>.
type REPLError = (FilePath, SourcePos , String)


-------------------------------
-- Initial Values
-------------------------------

-- | The initial state of the session.
initialST :: SessionState
initialST = ST 
    { errors      = []
    , loadedFiles = S.empty 
    , loadingMode = False
    , actualFile  = Nothing 
    , env         = False
    } 

-------------------------------
-- Utility Functions
-------------------------------

-- | Exec the applicative effect if the condition holds. Exactly the same as @flip when@
execIf :: Applicative f =>  f () -> Bool -> f ()
execIf = flip when

-- | Exec the applicative effect if the condition doesn't hold. Exactly the same as @flip unless@
execUnless ::  Applicative f =>  f () -> Bool -> f ()
execUnless = flip unless

-- | A way to print strings line by line
putStrLns :: Foldable t => t String -> IO ()
putStrLns = traverse_ putStrLn 

-- | Checks if the current session is in load mode.
isLoading :: (MonadState SessionState m) => m Bool
isLoading = loadingMode <$> get

-- | A way to show REPL errors.
showREPLError :: REPLError -> String 
showREPLError (fp,sp,err) = "<" ++ fp ++ "," ++ show sp ++ "," ++ err ++ ">"


-------------------------------
-- Parsing Functions
-------------------------------


{-
The REPL is going to have the following grammar (more or less):

<sentence> 
    = <special>
    | <code>

<special> = .load FilePath
            .failure 
            .reset
            .lex String
            .q
        
<code> = String

So the idea is to parse each sentence until we get a way to build state.
Since state is a monad, it's composable, thus the problem will be reduced to either
executing the state or folding (reducing) the state.
-}

-- | A parser for a file is just a parser for many lines.
parseFile :: Parser [StateT  SessionState IO ()]
parseFile = many parseLine

-- | A parser for a line is either a parser for the special commands
-- or in case that fails, a parser for the code.
parseLine :: Parser (StateT  SessionState IO ())
parseLine = parseSpecial <|> parseCode

-- | A parser for special commands is just parsing '.' and then
-- try parsing each possible command.
parseSpecial :: Parser (StateT  SessionState IO ())
parseSpecial = char '.' >> foldl1 (<|>)
    [ try parseFailure
    , try parseReset
    , try parseQuit
    , char 'l' >> (parseLex <|> parseLoad)
    ]

-- | TODO
parseCode :: Parser (StateT  SessionState IO ())
parseCode = do 
    string "x" 
    return failureState

-- | A parser for the failed command is just a parser for
-- the string "failed"
parseFailure :: Parser (StateT  SessionState IO ())
parseFailure = do
    string "failed"
    spaces
    return failureState

-- | The way failed manages state is by just printing the list of errors.
failureState :: StateT  SessionState IO ()
failureState = do
    err <- errors <$> get
    --execUnless (liftIO $ putStrLns $ map showREPLError err) =<< isLoading
    liftIO $ putStrLns $ map showREPLError err

-- | A parser for the reset command is just a parser for the string
-- "reset"
parseReset :: Parser (StateT  SessionState IO ())
parseReset = do
    string "reset"
    spaces
    return resetState

-- | The way reset manages state is by setting the list of errors
-- to the empty list unless we are in loading mode.
resetState :: StateT SessionState IO ()
resetState = execUnless (modify (\st -> st{errors=[]})) =<< isLoading

-- | A parser for the quit command is just a parser for the string
-- "q".
parseQuit :: Parser (StateT  SessionState IO ())
parseQuit = do
    string "q"
    spaces
    return quitState

-- | The way quite manages state is by lifting an @ExitSuccess@.
quitState :: StateT SessionState IO ()
quitState = liftIO exitSuccess 


-- | A parser for the lex command is just a parser for the string
-- "ex" (since l was read before) and a parser for the argument
-- to the lex command.
parseLex :: Parser (StateT  SessionState IO ())
parseLex = do
    string "ex" 
    spaces
    args <- parseArg
    pos  <-  getPosition 
    case parse manyToken "" args of
        Left parseError -> return $ lexFailureState pos parseError 
        Right tokens    -> return $ lexSuccessState args tokens  

-- | The way a success lexing manages state is by just printing to the
-- stdout the result.
lexSuccessState :: String -> [TokenPos] ->  StateT  SessionState IO ()
lexSuccessState args tokens = liftIO $ putStrLn $ showTokenPos args tokens--execIf (liftIO $ putStrLn $ showTokenPos args tokens)  =<< isLoading

-- | The way a failed lexing manages state is by adding the error to the list of errors
-- and then printing the error to the stdout.
lexFailureState :: SourcePos  -> ParseError ->  StateT  SessionState IO ()
lexFailureState oldPos parseError = do
    let ePos = errorPos parseError
    let (line,col) = ((,) <$> sourceLine <*> sourceColumn) oldPos
    let newLine = line + sourceLine ePos
    let newCol  = col  + sourceColumn ePos
    let newPos  = setSourceLine (setSourceColumn ePos  newCol ) newLine
    fp <- fromMaybe "." . actualFile <$> get  
    err <- errors <$> get
    let errors = last $ messageString <$> errorMessages parseError
    let replerrors = (fp,newPos,)  errors
    --execIf (liftIO $ putStrLn $ showREPLError replerror)  =<< isLoading
    --execIf (modify (\st -> st{errors=replerror:err})) =<< isLoading
    --liftIO $ putStrLns $ (\(_,_,e) -> e) <$> replerrors
    liftIO $ putStrLn $ (\(_,_,e) -> e) replerrors
    modify (\st -> st{errors=replerrors:err})

-- | A parser for the arguments of a @.load@ or @.lex@ is just parsing till
-- we reach either an end of line or an end of file.
parseArg :: Parser String
parseArg = manyTill anyChar (try (void endOfLine) <|> eof)

-- | A parser for the load command is just a parser for the string
-- "oad" (since 'l' eas previously parsed). (why did i do this? i don't  know, But i'll revert the change eventually...)
parseLoad :: Parser (StateT  SessionState IO ())
parseLoad = do
    string "oad"
    spaces
    loadState <$> parseArg

-- | The way load manages state is by parsing the file provided  by the user and then
-- updating the list of errors.
loadState :: String -> StateT SessionState IO ()
loadState fp = do
    fContents <-  liftIO $ unpack <$> readFile fp
    let pAction = parse parseFile fp fContents 
    case pAction of
        Left parseError -> error "WA" --trace "AQUI E" liftIO $ putStrLn $ show $ messageString <$> errorMessages parseError
        Right states     -> do
            let state =  sequence states
            newSession <- liftIO $ foldM (flip execStateT) initialST{loadingMode=True,actualFile=Just fp} states
            prev_errs <- errors   <$> get
            --newSessionErr <- liftIO $ errors <$> execStateT state initialST{loadingMode=True,actualFile=Just fp}
            let newSessionErr = errors newSession
            --execUnless (modify (\st -> st{errors=newSessionErr++prev_errs})) =<< isLoading
            modify (\st -> st{errors=newSessionErr++prev_errs})


-------------------------------
-- REPL 
-------------------------------

-- | The way repl' manages state is by just querying the user for a line
-- and parsing it to get the resulting state.
repl' :: StateT SessionState IO () 
repl' = do
    s      <- get
    liftIO $ putStr ">>> "
    iInput <- liftIO getLine 
    let pAction = parse parseLine "" iInput
    case pAction of
        Left parseError -> liftIO $ putStrLn $ ("Error!\n" ++) $ head $ messageString <$> errorMessages parseError
        Right state     -> state


-- | The REPL.
repl :: SessionState -> IO ()
repl st = do
    hSetBuffering stdout NoBuffering
    execStateT repl' st >>= repl