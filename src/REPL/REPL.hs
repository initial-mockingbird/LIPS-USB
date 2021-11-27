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

import           Control.Applicative    (Alternative (empty))
import           Control.Exception      (Exception (displayException),
                                         IOException, catch)
import           Control.Monad          (foldM_, unless, when)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.ByteString        (ByteString, readFile)
import           Data.ByteString.Char8  (unpack)
import           Data.Either            (Either (..), either)
import           Data.Foldable          (sequenceA_, traverse_)
import           Data.Functor           (($>))
import           Data.Maybe             (fromMaybe)
import           Lexer.Lexer            (manyToken, showTokenPos)
import           Prelude                hiding (lex, lines, readFile)
import           System.Exit            (exitSuccess)
import           System.IO              (BufferMode (NoBuffering),
                                         hSetBuffering, stdout)
import           Text.Parsec            (ParseError, ParsecT, SourcePos,
                                         alphaNum, anyChar, char, endOfLine,
                                         eof, errorPos, getInput, getPosition,
                                         getState, incSourceLine, letter, many,
                                         many1, manyTill, modifyState, newline,
                                         notFollowedBy, optional, parse,
                                         runParserT, sepEndBy, setSourceColumn,
                                         sourceLine, sourceName, spaces, string,
                                         try, (<?>), (<|>), setPosition)
import           Text.Parsec.Char       (alphaNum, anyChar, char, crlf,
                                         endOfLine, newline, spaces, string)
import           Text.Parsec.Error      (errorMessages, messageString)
import qualified HGrammar.HGrammar as H
import qualified Parser.Parser as P 
import qualified PParser.PParser as PP
import AST.AST
------------------------------
-- Types
------------------------------

-- | Holds the current state of the REPL.
data SessionState = ST 
    { errors      :: [REPLError]    -- ^ A list of all the errors so far.
    , loadingMode :: Bool           -- ^ Determines if the current state is originated from a @.load@ call
    , actualFile  :: Maybe FilePath -- ^ The file that was loaded by the @.load@ call
    , env         :: Bool           -- ^ We need an environment to hold the variables in the future
    } deriving Show


-- | Errors are formated as triples: <File name, Position, error msg>.
type REPLError = (FilePath, SourcePos , String)

-- | A parser for our REPL, holds the session state
type StateParser a = ParsecT String SessionState IO a

-------------------------------
-- Initial Values
-------------------------------

-- | The initial state of the session.
initialST :: SessionState
initialST = ST 
    { errors      = []
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


-- | A way to show REPL errors.
showREPLError :: REPLError -> String 
showREPLError (fp,sp,err) = "<" ++ fp ++ "," ++ show sp ++ "," ++ err ++ ">"

-- | A way to add an error to our inner state.
putErr :: REPLError -> StateParser ()
putErr err = do
    prevErrs <- errors <$> getState 
    modifyState (\st -> st{errors=err:prevErrs})

-- | A way to add many errors to our inner state.
putErrs :: Foldable t => t REPLError -> StateParser ()
putErrs = traverse_  putErr 

-- | Checks if we are inside a loaded file.
isLoading :: StateParser Bool
isLoading = loadingMode <$> getState 

-- | Execute action discard the result
void :: Functor f => f a -> f ()
void f = f $> ()

-- | Safely read from a file, returning an exception if we encounter a mistake.
safeReadFile :: FilePath -> IO (Either IOException ByteString)
safeReadFile p = (Right <$> readFile p) `catch` (pure . Left)

-- | Get the current working file from our state.
getCurrentFile :: StateParser String
getCurrentFile = fromMaybe "." . actualFile <$> getState 

-------------------------------
-- Parsing Functions
-------------------------------


{-
The REPL is going to have the following grammar (more or less):

<sentence> 
    = <special>
    | <code>
    | lambda (empty string)

<special> = .load FilePath
            .failure 
            .reset
            .lex String
            .q
        
<code> = String

So the idea is to parse each sentence until we get a way to modify the inner parser state.
-}


-- | A parser for a file is just a parser for many lines
-- but remembering only the last state.
parseFile :: StateParser [REPLError]
parseFile = concat . take 1 . reverse . fmap errors <$> parseFile'

-- | A parser for a file is just a parser for many lines.
parseFile' :: StateParser [SessionState]
parseFile' = many parseLine

-- | A parser that ignores empty input.
passEmpty :: StateParser a -> StateParser a
passEmpty s = do
    spaces
    optional  (try endOfLine)
    s

-- | A parser for a line is either a parser for the special commands
-- or in case that fails, a parser for the code.
parseLine :: StateParser SessionState
parseLine =  passEmpty $ parseSpecial <|> (parseCode >> getState)


-- | A parser for special commands is just parsing '.' and then
-- try parsing each possible command.
parseSpecial :: StateParser SessionState
parseSpecial = char '.' >> foldl1 (<|>)
    [ try parseFailure >> getState 
    , try parseReset   >> getState 
    , try parseQuit  
    , try parseParse2 >> getState
    , try parseParse3 >> getState  
    , try parseParse  >> getState
    , char 'l' >> (parseLex <|> parseLoad ) >> getState 
    ] <?> "ERROR: bad command"




-- | A parser for the @.failed@ command is just a parser for "failed". Prints to the stdout
-- the errors.
parseFailure :: StateParser ()
parseFailure = do
    string "failed"
    spaces
    notFollowedBy anyChar <?> "ERROR: unexpected argument provided to failed"
    err <- errors <$> getState
    liftIO $ putStrLns $ showREPLError <$> err

-- | A parser for the @.reset@ command is just a parser for "reset". Flushes
-- the errors unless its executed inside a file (it's generally not desirable to flush the error
-- list inside a @.load@ command since it WILL shadow errors)
parseReset :: StateParser ()
parseReset = do 
    string "reset"
    spaces 
    notFollowedBy anyChar <?> "ERROR: unexpected argument provided to reset"
    execUnless (modifyState (\st -> st{errors=[]} )) =<< isLoading


-- | A parser for the @.quit@ command is just a parser for the string
-- "q". It halts the execution of the REPL.
parseQuit :: StateParser a
parseQuit = do
    try (string "quit") <|> string "q"
    spaces
    notFollowedBy anyChar <?> "ERROR: unexpected argument provided to quit"
    liftIO exitSuccess 

-- | A parser for an error is just a way to update the error list
-- taking into consideration the previous position. 
parserError :: SourcePos -> ParseError  -> StateParser ()
parserError pos parseError = do
    let errMsg = messageString $ last $ errorMessages parseError
    let errPos = incSourceLine (errorPos parseError) (sourceLine pos - 1)
    let f      = sourceName errPos
    let tErr   = (f,errPos,errMsg)
    putErr tErr
    liftIO $ putStrLn $ showREPLError tErr

-- | A parser for the @.lex@ command is just a parser for the string
-- "ex" (since l was read before) and a parser for the argument
-- to the lex command.
parseLex :: StateParser ()
parseLex = do
    string "ex" 
    spaces
    pos  <- getPosition 
    args <- parseArg
    f    <- getCurrentFile
    let errFunction (errMsg,col) = (f,setSourceColumn pos col,errMsg)
    case manyToken args of
        Left errors -> do
            f <- fromMaybe "." . actualFile <$> getState 
            let mappedTriples =  (\ (errMsg,col) -> (f,setSourceColumn pos col,errMsg)) <$> errors
            mapM_ putErr mappedTriples
            mapM_ (liftIO . putStrLn . showREPLError) mappedTriples
        Right tokens          -> liftIO $ putStrLn $ showTokenPos args tokens 

-- | A parser for the @.parse@ command is just a parser for the string "ast", followed
-- by the execution of the parse command.
parseParse :: ParsecT String SessionState IO ()
parseParse = do
    string "ast"
    spaces
    pos'' <- getPosition 
    args  <- parseArg
    case H.parse args of
        Left (error,pos) -> do
            f <- fromMaybe "." . actualFile <$> getState 
            let pos' = setSourceColumn pos'' pos
            let mappedTriples =  (\ (errMsg) -> (f,pos' ,errMsg)) error
            putErr mappedTriples
            (liftIO . putStrLn . showREPLError) mappedTriples
        Right  s -> liftIO $ putStrLn $ showAST s


-- | A parser for the @.parse@ command is just a parser for the string "ast", followed
-- by the execution of the parse command.
parseParse3 :: ParsecT String SessionState IO ()
parseParse3 = do
    string "ast3"
    spaces
    f <- fromMaybe "." . actualFile <$> getState 
    es <- PP.parse f
    case es of
        Left (err,pos) -> do 
            f <- fromMaybe "." . actualFile <$> getState 
            setPosition pos
            let mappedTriples =  (\ (errMsg) -> (f,pos ,errMsg)) err
            putErr mappedTriples
            (liftIO . putStrLn . showREPLError) mappedTriples

        Right s  -> liftIO $ prettyPrintS  s
    
        


parseParse2 :: ParsecT String SessionState IO ()
parseParse2 = do
    string "ast2"
    spaces
    pos'' <- getPosition 
    args  <- parseArg
    liftIO $ putStrLn $ P.printASR_parserAMStart args

-- | A parser for the arguments of a @.load@ or @.lex@ is just parsing till
-- we reach either an end of line or an end of file.
parseArg :: StateParser String
parseArg = do
    fc <- manyTill anyChar (eof <|> void (try crlf) <|> void newline)
    return  fc


-- | A parser for the @.load@ command is just a parser for "oad" (since 'l' was read)
-- outputs an error if it cannot read the file, otherwise, it execute the file (almost) as if it
-- was executed in the REPL (see 'parseReset' for the difference).
parseLoad :: StateParser ()
parseLoad = do
    string "oad"
    spaces
    arg  <- parseArg <?> "ERROR: load ==> Expecting a file."
    fp   <- liftIO $ safeReadFile arg
    pos  <- getPosition 
    let ist = initialST{loadingMode=True,actualFile=Just arg}
    case fp of
        Left e     -> do
            let error = "ERROR: load ==> " ++ displayException  e
            f <- getCurrentFile
            let tError = (f,pos,error)
            liftIO $ putStrLn error
            putErr tError
        Right content -> do
            r <- liftIO $ runParserT parseFile ist arg (unpack content)
            either (parserError pos) putErrs r
            return ()
    
    
-- | A parse for the code is (temporarily) just an error.
parseCode :: StateParser ()
parseCode = do
    i    <- getInput 
    fc   <- alphaNum <?>  ("ERROR: " ++ show i ++ " ==> implementacion no implementada" )
    args <- parseArg
    pos  <- getPosition 
    f    <- getCurrentFile
    let err = "ERROR: " ++ (fc:args) ++ " ==> implementacion no implementada"
    putErr (f,pos,err)
    liftIO $ putStrLn err 



-------------------------------
-- REPL 
-------------------------------

-- | The REPL.
repl :: SessionState -> IO ()
repl st@ST{errors=errs} = do
    hSetBuffering stdout NoBuffering
    putStr ">>> "
    iInput <-  getLine 
    when (null iInput) $
         repl st
    res <- runParserT parseLine st "." iInput
    case res of
        Left parseError -> do
            let errMsg = messageString $ last $ errorMessages parseError
            let errPos = errorPos parseError
            let f      = sourceName errPos
            let tErr   = (f,errPos,errMsg)
            putStrLn $ showREPLError tErr
            repl st{errors=tErr:errs}
        Right st'       -> repl st'
        