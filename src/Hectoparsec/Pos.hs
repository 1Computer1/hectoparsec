{-|
Module      : Hectoparsec.Pos
Copyright   : (c) comp 2020
License     : MIT
Maintainer  : onecomputer00@gmail.com
Stability   : stable
Portability : portable

Data type for source position.
-}
module Hectoparsec.Pos
    ( -- * Positional data
      Pos(..)
    , initialPos
    , updatePos
    ) where

-- | The position of a token within the source text.
data Pos = Pos
    { posFile   :: !FilePath           -- ^ The source file path.
    , posLine   :: {-# UNPACK #-} !Int -- ^ The line number, starting at 1.
    , posColumn :: {-# UNPACK #-} !Int -- ^ The column number, starting at 1.
    }
    deriving (Show, Eq)

instance Ord Pos where
    compare (Pos _ l1 c1) (Pos _ l2 c2) = compare l1 l2 <> compare c1 c2

-- | Creates a 'Pos' for the start of a source text (line 1, column 1) with a given file path.
initialPos :: FilePath -> Pos
initialPos fp = Pos fp 1 1
{-# INLINE initialPos #-}

{-|
Updates position based on a character-like token.

Tokens which are considered newlines will increment the line number by 1 and set the column number to 1. Otherwise,
the column number is incremented by one.
-}
updatePos
    :: (a -> Bool) -- ^ Whether the token is a new line.
    -> a           -- ^ The token.
    -> Pos         -- ^ The position to update.
    -> Pos
updatePos isNewline tok (Pos fp line col)
    | isNewline tok = Pos fp (line + 1) 1
    | otherwise     = Pos fp line (col + 1)
{-# INLINE updatePos #-}
