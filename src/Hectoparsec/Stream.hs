{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-|
Module      : Hectoparsec.Stream
Copyright   : (c) comp 2020
License     : MIT
Maintainer  : onecomputer00@gmail.com
Stability   : stable
Portability : portable

Typeclass for manipulating input streams.

By default, textual input streams are supported by Hectoparsec. Custom streams, like token lists or lexers can also
made into a @'Stream'@:

@
import Data.Bifunctor (second)
import Data.List (uncons)

data Tok = Tok
    { tokSpan  :: ('Pos', 'Pos') -- Start and end span of the token.
    , tokValue :: String
    }

newtype TokStream = TokStream [Tok]

instance 'Stream' TokStream where
    type 'Token' TokStream = Tok
    type 'Chunk' TokStream = [Tok]

    'streamUncons' (TokStream xs) = second TokStream \<$> uncons xs
    'updatePosToken' _ tok _ = snd (tokSpan tok)
@
-}
module Hectoparsec.Stream
    ( -- * Stream typeclass
      Stream(..)
    ) where

import           Data.Bifunctor
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Proxy
import           Data.Word
import           Data.List
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Hectoparsec.Pos

{-|
A 'Stream' represents an input stream. These streams can be acted on one token at a time, or with multiple tokens at
a time in a chunk. Each token should represent a span of text (possibly just a character) in the source code.
-}
class Stream s where
    -- | The token type. This is a single element in your stream.
    type Token s

    -- | The type of chunks. This is a sequence of tokens in your stream.
    type Chunk s

    -- | Take the next token out of the stream. If the stream is empty, return 'Nothing'.
    streamUncons :: s -> Maybe (Token s, s)

    {-|
    Take at most the next /n/ tokens out of the stream. If /n/ is negative, return an empty chunk.

    By default, this repeatedly calls 'streamUncons', which may be ineffecient.
    -}
    streamSplitAt :: Int -> s -> (Chunk s, s)
    streamSplitAt n xs = first (tokensToChunk proxy) $ go n xs
        where
            go !m z | m <= 0 = ([], z)
            go !m z = case streamUncons z of
                    Nothing -> ([], z)
                    Just (x, z') -> first (x :) $ go (m - 1) z'

            proxy = Proxy :: Proxy s

    {-|
    Take tokens as long as a predicate holds.

    By default, this repeatedly calls 'streamUncons', which may be ineffecient.
    -}
    streamSpan :: (Token s -> Bool) -> s -> (Chunk s, s)
    streamSpan p xs = first (tokensToChunk proxy) $ go xs
        where
            go z = case streamUncons z of
                Just (x, z') | p x -> first (x :) $ go z'
                _ -> ([], z)

            proxy = Proxy :: Proxy s

    -- | Converts a chunk to tokens. This should be an isomorphism with 'tokensToChunk'.
    chunkToTokens :: proxy s -> Chunk s -> [Token s]

    default chunkToTokens :: Chunk s ~ [Token s] => proxy s -> Chunk s -> [Token s]
    chunkToTokens _ x = x

    -- | Converts tokens to a chunk. This should be an isomorphism with 'chunkToTokens'.
    tokensToChunk :: proxy s -> [Token s] -> Chunk s

    default tokensToChunk :: Chunk s ~ [Token s] => proxy s -> [Token s] -> Chunk s
    tokensToChunk _ x = x

    {-|
    Gets the length of a chunk.

    By default, this converts the chunk to tokens, which may be inefficient.

    > chunkLength proxy xs = length (chunkToTokens proxy xs)
    -}
    chunkLength :: proxy s -> Chunk s -> Int
    chunkLength proxy xs = length (chunkToTokens proxy xs)
    {-# INLINE chunkLength #-}

    {-|
    Checks whether a chunk is empty.

    By default, this converts the chunk to tokens, which may be inefficient.

    > chunkNull proxy xs = chunkLength proxy xs == 0
    -}
    chunkNull :: proxy s -> Chunk s -> Bool
    chunkNull proxy xs = chunkLength proxy xs == 0
    {-# INLINE chunkNull #-}

    {-|
    Performs a fold over a chunk.

    By default, this converts the chunk to tokens then folds over the list. There might be a better performing
    function for your custom stream type.
    -}
    foldChunk :: proxy s -> (b -> Token s -> b) -> b -> Chunk s -> b
    foldChunk proxy f z xs = foldl' f z (chunkToTokens proxy xs)
    {-# INLINE foldChunk #-}

    -- | Increments position according to a token.
    updatePosToken :: proxy s -> Token s -> Pos -> Pos

    -- | Increments position according to a chunk.
    updatePosChunk :: proxy s -> Chunk s -> Pos -> Pos
    updatePosChunk proxy xs p = foldChunk proxy (flip $ updatePosToken proxy) p xs
    {-# INLINE updatePosChunk #-}

    {-# MINIMAL streamUncons, updatePosToken #-}

instance Stream String where
    type Token String = Char
    type Chunk String = String

    streamUncons xs = uncons xs
    streamSplitAt n xs = splitAt n xs
    streamSpan p xs = span p xs
    chunkToTokens _ xs = xs
    tokensToChunk _ xs = xs
    chunkLength _ xs = length xs
    chunkNull _ xs = null xs
    foldChunk _ f z xs = foldl' f z xs
    updatePosToken _ t p = updatePos (== '\n') t p

instance Stream T.Text where
    type Token T.Text = Char
    type Chunk T.Text = T.Text

    streamUncons xs = T.uncons xs
    streamSplitAt n xs = T.splitAt n xs
    streamSpan p xs = T.span p xs
    chunkToTokens _ xs = T.unpack xs
    tokensToChunk _ xs = T.pack xs
    chunkLength _ xs = T.length xs
    chunkNull _ xs = T.null xs
    foldChunk _ f z xs = T.foldl' f z xs
    updatePosToken _ t p = updatePos (== '\n') t p

instance Stream TL.Text where
    type Token TL.Text = Char
    type Chunk TL.Text = TL.Text

    streamUncons xs = TL.uncons xs
    streamSplitAt n xs = TL.splitAt (fromIntegral n) xs
    streamSpan p xs = TL.span p xs
    chunkToTokens _ xs = TL.unpack xs
    tokensToChunk _ xs = TL.pack xs
    chunkLength _ xs = fromIntegral $ TL.length xs
    chunkNull _ xs = TL.null xs
    foldChunk _ f z xs = TL.foldl' f z xs
    updatePosToken _ t p = updatePos (== '\n') t p

instance Stream B.ByteString where
    type Token B.ByteString = Word8
    type Chunk B.ByteString = B.ByteString

    streamUncons xs = B.uncons xs
    streamSplitAt n xs = B.splitAt n xs
    streamSpan p xs = B.span p xs
    chunkToTokens _ xs = B.unpack xs
    tokensToChunk _ xs = B.pack xs
    chunkLength _ xs = B.length xs
    chunkNull _ xs = B.null xs
    foldChunk _ f z xs = B.foldl' f z xs
    updatePosToken _ t p = updatePos (== 10) t p

instance Stream BL.ByteString where
    type Token BL.ByteString = Word8
    type Chunk BL.ByteString = BL.ByteString

    streamUncons xs = BL.uncons xs
    streamSplitAt n xs = BL.splitAt (fromIntegral n) xs
    streamSpan p xs = BL.span p xs
    chunkToTokens _ xs = BL.unpack xs
    tokensToChunk _ xs = BL.pack xs
    chunkLength _ xs = fromIntegral $ BL.length xs
    chunkNull _ xs = BL.null xs
    foldChunk _ f z xs = BL.foldl' f z xs
    updatePosToken _ t p = updatePos (== 10) t p
