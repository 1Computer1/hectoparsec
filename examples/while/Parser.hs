{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Parser
    ( Stmt(..)
    , Expr(..)
    , Op(..)
    , TokStream(..)
    , CustomError(..)
    , CustomLabel(..)
    , pModule
    ) where

import           Control.Monad.Combinators
import           Control.Monad.Writer.Strict
import           Data.List
import qualified Data.Text as T
import           Hectoparsec

import           Lexer

---------
-- AST --
---------

data Stmt
    = StmtLet (L T.Text) (L Expr)                  -- `let x = e;`
    | StmtIf [(L Expr, [L Stmt])] (Maybe [L Stmt]) -- `if (a) { .. } else if (b) { .. } else { .. }`
    | StmtWhile (L Expr) [L Stmt]                  -- `while (e) { .. }`
    | StmtReturn (L Expr)                          -- `return e;`
    | StmtFn (L T.Text) [L T.Text] [L Stmt]        -- `fn foo(x, y) { .. }`
    | StmtExpr (L Expr)                            -- `e;`
    deriving (Show)

-- Notice the overlap between 'ExprArrow', 'ExprTuple', and 'ExprGrouped'.
data Expr
    = ExprBin (L Expr) (L Op) (L Expr) -- `a + b`
    | ExprCall (L Expr) [L Expr]       -- `f(a)`
    | ExprArrow [L T.Text] (L Expr)    -- `(x, y) -> e`
    | ExprTuple [L Expr]               -- `(a, b)`
    | ExprGrouped (L Expr)             -- `(e)`
    | ExprAssign (L T.Text) (L Expr)   -- `x = e`
    | ExprInt (L Integer)              -- `123`
    | ExprStr (L T.Text)               -- `"hello"`
    | ExprVar (L T.Text)               -- `x`
    deriving (Show)

data Op = OpAdd | OpSub | OpMul | OpDiv | OpEqual
    deriving (Show)

-------------
-- Parsers --
-------------

-- | Our stream type.
newtype TokStream = TokStream [L Tok]

instance Stream TokStream where
    type Token TokStream = L Tok
    type Chunk TokStream = [L Tok]

    streamUncons (TokStream xs) = fmap TokStream <$> uncons xs

    updatePosToken _ lt _ = let Span fp _ (l, c) = lspan lt in Pos fp l c

-- | A custom error we can report. We will use some of these as warnings to report during parsing.
data CustomError
    = ErrorConstantCondition (L Expr) -- ^ An error for when we detect `if (1)` or similar.
    | ErrorWhileLoop (L Stmt)         -- ^ An error to not use while loops, because recursion is cooler.
    | ErrorArrowBrace (L Tok)         -- ^ An error when there is a `{` after a `->`, since we only allow expressions.
    deriving (Show)

-- | A custom label to add on parsers.
data CustomLabel
    = LabelTok Tok    -- ^ A label for an expected token.
    | LabelInt        -- ^ A label for literal integers.
    | LabelStr        -- ^ A label for literal strings.
    | LabelIdent      -- ^ A label for identifiers.
    | LabelExpression -- ^ A label for the expression parser.
    | LabelStatement  -- ^ A label for the statement parser.
    deriving (Show, Eq, Ord)

-- | Parser type with token stream, custom errors, and custom labels.
-- We also compose 'Writer' in order to report warnings during parsing.
type P = ParserT TokStream CustomError CustomLabel (Writer [ParseError TokStream CustomError CustomLabel])

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

reportWarning :: CustomError -> P ()
reportWarning ce = do
    st <- getState
    let e = makeErrorAt st (ErrorItemMessages [MessageCustom ce])
    tell [e] -- Not super efficient, but it'll do.

pModule :: P [L Stmt]
pModule = many pStmt <* token TokEof

pStmt :: P (L Stmt)
pStmt = label LabelStatement $ choice
    [ pLet
    , pIf
    , pWhile
    , pFn
    , pReturn
    , pStmtExpr
    ]

pLet :: P (L Stmt)
pLet = do
    t0 <- token (TokKw "let")
    ident <- pTokIdent
    token TokEq
    e <- pExpr
    s <- token TokSemi
    pure $ L (lspan t0 <> lspan s) (StmtLet ident e)

pIf :: P (L Stmt)
pIf = do
    t0 <- token (TokKw "if")
    cb0 <- pCondBody

    branches <- many . try $ do
        token (TokKw "else")
        token (TokKw "if")
        pCondBody

    elseBranch <- optional $ do
        token (TokKw "else")
        token TokLBrace
        body <- many pStmt
        rb <- token TokRBrace
        pure (body, lspan rb)

    let cbs = cb0 : branches
    let fin = maybe (snd $ last cbs) snd elseBranch
    pure $ L (lspan t0 <> fin) (StmtIf (map fst cbs) (fst <$> elseBranch))
    where
        pCondBody = do
            token TokLParen
            cond <- pExpr
            case cond of
                L _ (ExprInt (L _ 1)) -> reportWarning $ ErrorConstantCondition cond
                _ -> pure ()

            token TokRParen
            token TokLBrace
            body <- many pStmt
            rb <- token TokRBrace
            pure $ ((cond, body), lspan rb)

pWhile :: P (L Stmt)
pWhile = do
    while <- token (TokKw "while")
    token TokLParen
    cond <- pExpr
    case cond of
        L _ (ExprInt (L _ 1)) -> reportWarning $ ErrorConstantCondition cond
        _ -> pure ()

    token TokRParen
    token TokLBrace
    body <- many pStmt
    rb <- token TokRBrace
    let w = L (lspan while <> lspan rb) (StmtWhile cond body)
    reportWarning $ ErrorWhileLoop w
    pure w

pFn :: P (L Stmt)
pFn = do
    fn <- token (TokKw "fn")
    ident <- pTokIdent
    token TokLParen
    xs <- pTokIdent `sepEndBy` token TokComma
    token TokRParen
    token TokLBrace
    body <- many pStmt
    rb <- token TokRBrace
    pure $ L (lspan fn <> lspan rb) (StmtFn ident xs body)

pReturn :: P (L Stmt)
pReturn = do
    ret <- token (TokKw "return")
    e <- pExpr
    s <- token TokSemi
    pure $ L (lspan ret <> lspan s) (StmtReturn e)

pStmtExpr :: P (L Stmt)
pStmtExpr = do
    e <- pExpr
    s <- token TokSemi
    pure $ L (lspan e <> lspan s) (StmtExpr e)

pExpr :: P (L Expr)
pExpr = pEqual <?> LabelExpression
    where
        pEqual = pBinOp [(TokEqEq, OpEqual)] pMul
        pMul = pBinOp [(TokTimes, OpMul), (TokDivide, OpDiv)] pAdd
        pAdd = pBinOp [(TokPlus, OpAdd), (TokMinus, OpSub)] pTerm
        pTerm = choice
            [ pArrow  -- This does not have a 'try' here. See 'pArrow' for why.
            , pAssign -- Similarly here.
            , pCall
            ]

pBinOp :: [(Tok, Op)] -> P (L Expr) -> P (L Expr)
pBinOp ops lower = do
    x <- lower
    xs <- many ((,) <$> pOps <*> lower)
    pure $ foldl (\a (o, e) -> L (lspan a <> lspan e) $ ExprBin a o e) x xs
    where
        pOps = choice $ map (\(t, o) -> (\(L s _) -> L s o) <$> token t) ops

pCall :: P (L Expr)
pCall = do
    f <- lower
    calls <- many $ do
        token TokLParen
        es <- pExpr `sepEndBy` token TokComma
        rp <- token TokRParen
        pure (es, lspan rp)
    case calls of
        [] -> pure f
        ((c0, r0):cs) ->
            pure $ foldl (\a (es, r) -> L (lspan a <> r) (ExprCall a es)) (L (lspan f <> r0) (ExprCall f c0)) cs
    where
        lower = choice
            [ try pGrouped
            , pTuple
            , pInt
            , pStr
            , pVar
            ]

pArrow :: P (L Expr)
pArrow = do
    -- The 'try' is here so that if 'pArrow' fails in the parameter list, we can assume the user meant a tuple or
    -- a grouped expression instead.
    (lp, xs) <- try $ do
        lp <- token TokLParen
        xs <- pTokIdent `sepEndBy` token TokComma
        token TokRParen
        token TokArrow
        pure (lp, xs)

    lb <- hidden . optional $ token TokLBrace
    case lb of
        Nothing -> pure ()
        Just x -> customError (ErrorArrowBrace x)

    -- If the previous block gets parsed, including the arrow, then we know the user meant an arrow function,
    -- so it can no longer backtrack if 'pExpr' fails next, giving us the correct error message.
    e <- pExpr
    pure $ L (lspan lp <> lspan e) (ExprArrow xs e)

pTuple :: P (L Expr)
pTuple = do
    lp <- token TokLParen
    es <- pExpr `sepEndBy` token TokComma
    rp <- token TokRParen
    pure $ L (lspan lp <> lspan rp) (ExprTuple es)

pGrouped :: P (L Expr)
pGrouped = do
    lp <- token TokLParen
    e <- pExpr
    rp <- token TokRParen
    pure $ L (lspan lp <> lspan rp) (ExprGrouped e)

pAssign :: P (L Expr)
pAssign = do
    -- Same reasoning as the 'try' in 'pArrow'.
    lhs <- try $ do
        lhs <- pTokIdent
        token TokEq
        pure lhs

    rhs <- pExpr
    pure $ L (lspan lhs <> lspan rhs) (ExprAssign lhs rhs)

pInt :: P (L Expr)
pInt = do
    n <- pTokInt
    pure $ L (lspan n) (ExprInt n)

pStr :: P (L Expr)
pStr = do
    str <- pTokStr
    pure $ L (lspan str) (ExprStr str)

pVar :: P (L Expr)
pVar = do
    ident <- pTokIdent
    pure $ L (lspan ident) (ExprVar ident)

pTokInt :: P (L Integer)
pTokInt = label LabelInt . tokenF $ \t ->
    case t of
        TokInt x -> Just x
        _ -> Nothing

pTokStr :: P (L T.Text)
pTokStr = label LabelStr . tokenF $ \t ->
    case t of
        TokStr x -> Just x
        _ -> Nothing

pTokIdent :: P (L T.Text)
pTokIdent = label LabelIdent . tokenF $ \t ->
    case t of
        TokIdent x -> Just x
        _ -> Nothing
