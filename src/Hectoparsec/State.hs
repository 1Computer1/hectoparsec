{-|
Module      : Hectoparsec.State
Copyright   : (c) comp 2020
License     : MIT
Maintainer  : onecomputer00@gmail.com
Stability   : stable
Portability : portable

The parser state.
-}
module Hectoparsec.State
    ( -- * Parser state
      State(..)
    , initialState
    , makeErrorAt
    ) where

import Hectoparsec.Error
import Hectoparsec.Pos

-- | The parser state.
data State s = State
    { stateInput  :: s                   -- ^ The rest of the input to parse.
    , statePos    :: !Pos                -- ^ The current position in the source.
    , stateOffset :: {-# UNPACK #-} !Int -- ^ The current offset in the stream.
    }

-- | Creates a 'State' at the start of a stream.
initialState :: FilePath -> s -> State s
initialState fp s = State s (initialPos fp) 0
{-# INLINE initialState #-}

-- | Creates a parse error from an error item located by some parsing state.
makeErrorAt :: State s -> ErrorItem s e l -> ParseError s e l
makeErrorAt st err = ParseError (statePos st) (stateOffset st) err
{-# INLINE makeErrorAt #-}
