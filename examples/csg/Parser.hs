{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Parser
    ( Expr(..)
    , LexStream(..)
    , CustomLabel(..)
    , pExprTop
    ) where

import           Control.Monad.Combinators
import qualified Data.Text as T
import           Data.Void
import           Hectoparsec

import           Lexer

---------
-- AST --
---------

data Expr
    = ExprAdd (L Expr) (L Expr)                -- `a + b`
    | ExprInt (L Integer)                  -- `123`
    | ExprStr [Either (L Expr) (L T.Text)] -- `"hello $(1 + 1)"`
    | ExprGrouped (L Expr)                     -- `(a + b)`
    deriving (Show)

-------------
-- Parsers --
-------------

-- | Our stream type. It uses the token lexer to retrieve tokens.
data LexStream = LexStream
    { lexStreamState :: State T.Text -- The lexer state to use next time.
    , lexStreamModes :: [Mode]       -- The lexer modes to use next time.
    }

instance Stream LexStream where
    type Token LexStream = L Tok
    type Chunk LexStream = [L Tok]

    streamUncons (LexStream st ms) =
        let (st', r) = runParser (lexToken (head ms)) st
        in case r of
            Left _  -> Nothing
            Right t -> Just (t, LexStream st' ms)

    updatePosToken _ lt _ = let Span fp _ (l, c) = lspan lt in Pos fp l c

-- | A custom label to add on parsers.
data CustomLabel
    = LabelTok Tok    -- ^ A label for an expected token.
    | LabelInt        -- ^ A label for literal integers.
    | LabelStr        -- ^ A label for literal strings.
    | LabelExpression -- ^ A label on the expression parser.
    deriving (Show, Eq, Ord)

-- | Parser type with our lexer stream, no custom errors, and custom labels.
type P = Parser LexStream Void CustomLabel

-- | Our own token parser that uses 'Tok'.
token :: Tok -> P (L Tok)
token k = matchToken $ \m ->
    case m of
        Just lt | lval lt == k -> Right lt
        Just lt -> Left (ErrorItemLabels (UnexpectedToken lt) [LabelTok k])
        Nothing -> Left (ErrorItemLabels UnexpectedEnd [LabelTok k])

tokenF :: (Tok -> Maybe a) -> P (L a)
tokenF f = matchToken $ \m ->
    case m of
        Just (L s t) | Just x <- f t -> Right (L s x)
        Just lt -> Left (ErrorItemLabels (UnexpectedToken lt) [])
        Nothing -> Left (ErrorItemLabels UnexpectedEnd [])

pushMode :: Mode -> P ()
pushMode m = modifyInput $ \(LexStream st ms) -> LexStream st (m:ms)

popMode :: P ()
popMode = modifyInput $ \(LexStream st ms) -> LexStream st (tail ms)

pExprTop :: P (L Expr)
pExprTop = pExpr <* token TokEof

pExpr :: P (L Expr)
pExpr = label LabelExpression $ do
    x <- lower
    xs <- many (token TokPlus *> lower)
    pure $ foldl (\a e -> L (lspan a <> lspan e) $ ExprAdd a e) x xs
    where
        lower = choice [pInt, pStr, pGrouped]

pGrouped :: P (L Expr)
pGrouped = do
    lp <- token TokLParen
    e <- pExpr
    rp <- token TokRParen
    pure $ L (lspan lp <> lspan rp) (ExprGrouped e)

pInt :: P (L Expr)
pInt = do
    n <- pTokInt
    pure $ L (lspan n) (ExprInt n)

pTokInt :: P (L Integer)
pTokInt = label LabelInt . tokenF $ \t ->
    case t of
        TokInt x -> Just x
        _ -> Nothing

pStr :: P (L Expr)
pStr = label LabelStr $ do
    qs <- token TokQuote
    pushMode ModeString
    str <- mergeChunks <$> many pStrChunk
    qe <- token TokQuote
    popMode
    pure $ L (lspan qs <> lspan qe) (ExprStr str)
    where
        mergeChunks [] = []
        mergeChunks [x] = [x]
        mergeChunks (Right (L ss x) : Right (L se y) : xs) = mergeChunks $ Right (L (ss <> se) (x <> y)) : xs
        mergeChunks (x:xs) = x : mergeChunks xs

pStrChunk :: P (Either (L Expr) (L T.Text))
pStrChunk = eitherP (token TokLInterp *> pushMode ModeExpr *> pExpr <* token TokRParen <* popMode) pTokStr

pTokStr :: P (L T.Text)
pTokStr = tokenF $ \t ->
    case t of
        TokStr x -> Just x
        _ -> Nothing
