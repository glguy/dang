{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Syntax.ParserCore where

import Control.Applicative (Applicative)
import Data.Int (Int64)
import MonadLib
import qualified Data.ByteString as S


-- Lexer/Parser Monad ----------------------------------------------------------

data Position = Position
  { posOff  :: !Int
  , posLine :: !Int
  , posCol  :: !Int
  , posFile :: FilePath
  } deriving Show

initPosition :: FilePath -> Position
initPosition path = Position
  { posOff  = 0
  , posLine = 1
  , posCol  = 1
  , posFile = path
  }

movePos :: Position -> Char -> Position
movePos (Position a line col path) c =
  case c of
    '\t' -> Position (a+1) line (col+8) path
    '\n' -> Position (a+1) (line+1) col path
    _    -> Position (a+1) line (col+1) path

data Token
  = TReserved String
  | TIdent String
  | TInt Int64
  | TEof
    deriving (Eq,Show)

data Lexeme = Lexeme
  { lexPos   :: !Position
  , lexToken :: Token
  } deriving Show

instance Eq Lexeme where
  a == b = lexToken a == lexToken b

data ErrorType
  = LexerError
  | ParserError
    deriving Show

data Error = Error ErrorType String Position deriving Show

data ParserState = ParserState
  { psInput   :: !S.ByteString
  , psChar    :: !Char
  , psPos     :: !Position
  , psLexCode :: !Int
  } deriving Show

initParserState :: FilePath -> S.ByteString -> ParserState
initParserState path bs = ParserState
  { psInput   = bs
  , psChar    = '\n'
  , psPos     = initPosition path
  , psLexCode = 0
  }

newtype Parser a = Parser
  { unParser :: StateT ParserState (ExceptionT Error Id) a
  } deriving (Functor,Applicative,Monad)

instance StateM Parser ParserState where
  get = Parser   get
  set = Parser . set

instance ExceptionM Parser Error where
  raise = Parser . raise

instance RunExceptionM Parser Error where
  try m = Parser (try (unParser m))

-- | Raise an exception from the lexer.
raiseL :: String -> Parser a
raiseL msg = do
  st <- get
  raise (Error LexerError msg (psPos st))

-- | Raise an exception from the parser.
raiseP :: String -> Parser a
raiseP msg = do
  st <- get
  raise (Error ParserError msg (psPos st))

-- | Run the parser over the file given.
runParser :: FilePath -> S.ByteString -> Parser a -> Either Error a
runParser path bs (Parser m) =
  case runM m (initParserState path bs) of
    Right (a,_) -> Right a
    Left err    -> Left err