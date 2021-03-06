{-|
Module      : Hectoparsec
Copyright   : (c) comp 2020
License     : MIT
Maintainer  : onecomputer00@gmail.com
Stability   : stable
Portability : portable

Top-level module for Hectoparsec. These parsers are smart in keeping track of labels for parsers, which can be used
to generate informational error messages. They also keep track of source positions, which can be attached to results.

To get started, define a parser type for your specific usecase. A parser works over a certain input stream and can
use a custom error type or label type. Custom input streams can be used by implementing the 'Stream' typeclass.

@
import Control.Applicative
import Data.Text (Text)
import Data.Void (Void)
import Hectoparsec

-- Parser over a text stream, with no custom errors, and with string parser labels.
type P = 'Parser' Text Void String

data Color = Red | Green | Blue

red, green, blue :: P Color

red   = Red   \<$ 'string' \"red\"   '<?>' \"red\"
green = Green \<$ 'string' \"green\" '<?>' \"green\"
blue  = Blue  \<$ 'string' \"blue\"  '<?>' \"blue\"

color :: P Color
color = red \<|> green \<|> blue

parseColor :: FilePath -> Text -> Either ('ParseError' Text Void String) Color
parseColor fp s = 'evalParser' (color <* 'endOfInput') fp s
@
-}
module Hectoparsec
    ( -- * Re-exports
      module Hectoparsec.Error
    , module Hectoparsec.Pos
    , module Hectoparsec.Stream
    , module Hectoparsec.State
      -- * ParserT monad transformer
    , ParserT
    , evalParserT
    , runParserT
      -- * Parser monad
    , Parser
    , evalParser
    , runParser
      -- * MonadParser typeclass
    , MonadParser(..)
      -- ** Derived combinators
      -- *** Input consumption
    , anyToken
    , char
    , string
    , satisfy
    , peek
    , peekNext
    , countTokens
    , tokenWhile
    , tokenWhile1
    , matchRest
    , atEnd
      -- *** Label combinators
    , label
    , (<?>)
    , hidden
      -- *** Error combinators
    , restore
    , unexpected
    , failure
    , customError
      -- *** State combinators
    , getsState
    , modifyState
    , getInput
    , getsInput
    , putInput
    , modifyInput
    , getPos
    , getOffset
    ) where

import Data.Functor.Identity
import Hectoparsec.Class
import Hectoparsec.Error
import Hectoparsec.Pos
import Hectoparsec.Primitive
import Hectoparsec.State
import Hectoparsec.Stream

-- | Runs a parser given an input stream and the file name. Returns either the parse error or the result.
evalParserT
    :: Monad m
    => ParserT s e l m a
    -> FilePath
    -> s
    -> m (Either (ParseError s e l) a)
evalParserT p fp s = fmap snd $ runParserT p (initialState fp s)

-- | A variant of 'evalParserT' that takes in an initial state and also gives the final state.
runParserT
    :: Monad m
    => ParserT s e l m a
    -> State s
    -> m (State s, Either (ParseError s e l) a)
runParserT p st = do
    Reply st' _ res <- contParserT p st
    return $ case res of
        Right a -> (st', Right a)
        Left e  -> (st', Left e)

-- | Runs a parser given an input stream and the file name. Returns either the parse error or the result.
evalParser
    :: Parser s e l a
    -> FilePath
    -> s
    -> Either (ParseError s e l) a
evalParser p fp s = runIdentity $ evalParserT p fp s

-- | A variant of 'evalParser' that takes in an initial state and also gives the final state.
runParser
    :: Parser s e l a
    -> State s
    -> (State s, Either (ParseError s e l) a)
runParser p st = runIdentity $ runParserT p st
