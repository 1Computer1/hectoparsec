{-# LANGUAGE OverloadedStrings #-}

module Lexer
    ( L(..)
    , Span(..)
    , Tok(..)
    , prettyTok
    , lexer
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
    = TokIdent T.Text
    | TokKw T.Text
    | TokInt Integer
    | TokStr T.Text
    | TokArrow
    | TokLBrace
    | TokRBrace
    | TokLParen
    | TokRParen
    | TokEqEq
    | TokEq
    | TokPlus
    | TokMinus
    | TokTimes
    | TokDivide
    | TokSemi
    | TokComma
    | TokEof
    | TokEofComment   -- ^ This is for unterminated block comments.
    | TokUnknown Char -- ^ We have an unknown token to lex unknown characters. This means that the lexer cannot error.
    deriving (Show, Eq, Ord)

prettyTok :: Tok -> T.Text
prettyTok t = case t of
    TokIdent x -> x
    TokKw x -> x
    TokInt n -> T.pack (show n)
    TokStr x -> "string \"" <> x <> "\""
    TokArrow -> "`->`"
    TokLBrace -> "`{`"
    TokRBrace -> "`}`"
    TokLParen -> "`(`"
    TokRParen -> "`)`"
    TokEqEq -> "`==`"
    TokEq -> "`=`"
    TokPlus -> "`+`"
    TokMinus -> "`-`"
    TokTimes -> "`*`"
    TokDivide -> "`/`"
    TokSemi -> "`;`"
    TokComma -> "`,`"
    TokEof -> "end of input"
    TokEofComment -> "unterminated block comment"
    TokUnknown c -> "`" <> T.singleton c <> "`"

-------------
-- Parsers --
-------------

-- | Parser over a 'Text' stream, with no custom errors and no custom labels.
type P = Parser T.Text Void Void

located :: P a -> P (L a)
located p = do
    a <- getPos
    x <- p
    b <- getPos
    pure $ L (spanPos a b) x

lexer :: P [L Tok]
lexer = do
    t <- pToken
    case t of
        L _ TokEof        -> pure [t]
        L _ TokEofComment -> pure [t]
        _              -> (t :) <$> lexer

pToken :: P (L Tok)
pToken = choice
    [ pSpaces *> pToken
    , pComment *> pToken
    , pBlockComment
    , pLower
    , pInt
    , pStr
    , pSymbol
    , pUnknown
    , pEof
    ]

pSpaces :: P ()
pSpaces = void $ tokenWhile1 isSpace

pComment :: P ()
pComment = void $ string "//" *> tokenWhile (/= '\n')

pBlockComment :: P (L Tok)
pBlockComment = do
    string "/*"
    (_, x) <- manyTill_ anyToken $ choice
        [ string "*/" *> pToken
        , located $ TokEofComment <$ endOfInput
        ]
    pure x

pLower :: P (L Tok)
pLower = located $ do
    xs <- T.cons <$> satisfy isLower <*> tokenWhile isAlphaNum
    if xs `elem` ["return", "if", "else", "fn", "let", "while"]
        then pure $ TokKw xs
        else pure $ TokIdent xs

pInt :: P (L Tok)
pInt = located $ do
    xs <- tokenWhile1 isDigit
    pure $ TokInt (read (T.unpack xs))

pStr :: P (L Tok)
pStr = located $ do
    char '"'
    xs <- tokenWhile (/= '"')
    char '"'
    pure $ TokStr xs

pSymbol :: P (L Tok)
pSymbol = choice . map located $
    [ string "->" $> TokArrow
    , string "{" $> TokLBrace
    , string "}" $> TokRBrace
    , string "(" $> TokLParen
    , string ")" $> TokRParen
    , string "==" $> TokEqEq
    , string "=" $> TokEq
    , string "+" $> TokPlus
    , string "-" $> TokMinus
    , string "*" $> TokTimes
    , string "/" $> TokDivide
    , string ";" $> TokSemi
    , string "," $> TokComma
    ]

pUnknown :: P (L Tok)
pUnknown = located $ do
    x <- anyToken
    pure $ TokUnknown x

pEof :: P (L Tok)
pEof = located $ do
    endOfInput
    pure TokEof
