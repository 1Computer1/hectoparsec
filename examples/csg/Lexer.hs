{-# LANGUAGE OverloadedStrings #-}

module Lexer
    ( L(..)
    , Span(..)
    , Tok(..)
    , Mode(..)
    , lexToken
    ) where

import           Control.Monad
import           Control.Monad.Combinators
import           Data.Char
import           Data.Functor
import qualified Data.Text as T
import           Data.Void
import           Hectoparsec

-----------------
-- Token types --
-----------------

-- | A located item.
data L a = L
    { lspan :: Span
    , lval  :: a
    }
    deriving (Eq, Ord)

instance Show a => Show (L a) where
    show (L s a) = concat ["{", show s, " ", show a, "}"]

-- | Span in the source.
data Span = Span
    { spanFile  :: FilePath   -- ^ Filepath.
    , spanStart :: (Int, Int) -- ^ Starting line and column.
    , spanEnd   :: (Int, Int) -- ^ Ending line and column.
    }
    deriving (Eq, Ord)

spanPos :: Pos -> Pos -> Span
spanPos (Pos f1 l1 c1) (Pos _ l2 c2) = Span f1 (l1, c1) (l2, c2)

instance Show Span where
    show (Span _ (ls, cs) (le, ce)) = concat [show ls, ":", show cs, "-", show le, ":", show ce]

instance Semigroup Span where
    Span fp s1 e1 <> Span _ s2 e2 = Span fp (min s1 s2) (max e1 e2)

-- | A token.
data Tok
    = TokInt Integer
    | TokPlus
    | TokQuote
    | TokStr T.Text
    | TokLInterp
    | TokLParen
    | TokRParen
    | TokEof
    | TokEofComment
    | TokUnknown Char
    deriving (Show, Eq, Ord)

-------------
-- Parsers --
-------------

-- | Parser over a 'Text' stream, with no custom errors and no custom labels.
type P = Parser T.Text Void Void

data Mode
    = ModeExpr   -- ^ In this mode, we parse tokens for expressions.
    | ModeString -- ^ In this mode, we parse tokens for strings.
    deriving (Show, Eq)

located :: P a -> P (L a)
located p = do
    a <- getPos
    x <- p
    b <- getPos
    pure $ L (spanPos a b) x

-- | Lexes a token. We pass in the lexer mode, in order to lex different things depending on what the parser wants.
lexToken :: Mode -> P (L Tok)
lexToken mode =
    case mode of
        ModeExpr -> choice
            [ pSpaces *> lexToken mode
            , pComment *> lexToken mode
            , pBlockComment mode
            , pInt
            , located $ string "+" $> TokPlus
            , located $ string "\"" $> TokQuote
            , located $ string "(" $> TokLParen
            , located $ string ")" $> TokRParen
            , pUnknown
            , pEof
            ]
        ModeString -> choice
            [ located $ string "\"" $> TokQuote
            , located $ string "$(" $> TokLInterp
            , pEsc
            , pStr
            , pEof
            ]

pSpaces :: P ()
pSpaces = void $ tokenWhile1 isSpace

pComment :: P ()
pComment = void $ string "//" *> tokenWhile (/= '\n')

pBlockComment :: Mode -> P (L Tok)
pBlockComment mode = do
    string "/*"
    (_, x) <- manyTill_ anyToken $ choice
        [ string "*/" *> lexToken mode
        , located $ endOfInput $> TokEofComment
        ]
    pure x

pInt :: P (L Tok)
pInt = located $ do
    xs <- tokenWhile1 isDigit
    pure $ TokInt (read (T.unpack xs))

pStr :: P (L Tok)
pStr = located $ do
    -- The way this parses means we get stuttered chunks of text instead of clean chunks.
    -- Certainly, this can be done better, but this is good enough for an example.
    xs <- tokenWhile1 (\x -> x /= '"' && x /= '\\' && x /= '$')
    ds <- option "" (try $ string "$" <* notFollowedBy (char '('))
    pure $ TokStr $ xs <> ds

pEsc :: P (L Tok)
pEsc = located $ do
    char '\\'
    x <- anyToken
    pure $ TokStr (T.singleton x)

pUnknown :: P (L Tok)
pUnknown = located $ do
    x <- anyToken
    pure $ TokUnknown x

pEof :: P (L Tok)
pEof = located $ do
    endOfInput
    pure TokEof
