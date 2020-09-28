{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE UndecidableInstances   #-}

{-|
Module      : Hectoparsec.Class
Copyright   : (c) comp 2020
License     : MIT
Maintainer  : onecomputer00@gmail.com
Stability   : stable
Portability : portable

Typeclass for a monad that implements the basic combinators for parsing.

Combinators that are derived from the basic combinators are also defined here.
-}
module Hectoparsec.Class
    ( -- * MonadParser typeclass
      MonadParser(..)
      -- * Derived combinators
      -- ** Input consumption
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
      -- ** Label combinators
    , label
    , (<?>)
    , hidden
      -- ** Error combinators
    , restore
    , unexpected
    , failure
    , customError
      -- ** State combinators
    , getsState
    , modifyState
    , getInput
    , getsInput
    , putInput
    , modifyInput
    , getPos
    , getOffset
    ) where

import           Control.Monad
import           Control.Monad.Trans
import qualified Control.Monad.Trans.Accum as M
import qualified Control.Monad.Trans.Except as M
import qualified Control.Monad.Trans.Identity as M
import qualified Control.Monad.Trans.Maybe as M
import qualified Control.Monad.Trans.RWS.CPS as C
import qualified Control.Monad.Trans.RWS.Lazy as L
import qualified Control.Monad.Trans.RWS.Strict as S
import qualified Control.Monad.Trans.Reader as M
import qualified Control.Monad.Trans.State.Lazy as L
import qualified Control.Monad.Trans.State.Strict as S
import qualified Control.Monad.Trans.Writer.CPS as C
import qualified Control.Monad.Trans.Writer.Lazy as L
import qualified Control.Monad.Trans.Writer.Strict as S
import           Data.Proxy
import           Hectoparsec.Error
import           Hectoparsec.Pos
import           Hectoparsec.State
import           Hectoparsec.Stream

{-|
Monad @m@ that implements the primitive parsers for a stream @s@, using custom errors @e@ and custom labels @l@. These
parsers should have a notion of whether the input was consumed or not. They should also track the parser state, errors,
and labels.

The 'MonadPlus' instance should be equal to the 'Control.Applicative.Alternative' instance, and it should implement the
operations for branching parsers. In particular, @p 'Control.Applicative.<|>' q@ must commit to the first branch
that consumes input.

The 'Hectoparsec.ParserT' instance is the canonical instance for this class.
-}
class (Stream s, MonadPlus m) => MonadParser s e l m | m -> s e l where
    {-|
    Match on a token, returning either the value or an error. If this succeeds, input is consumed.

    For matching by equality, use the derived 'char' combinator.
    -}
    matchToken
        :: -- | A function to match on tokens. It can return either an error item or the resulting value.
           (Maybe (Token s) -> Either (ErrorItem s e l) a)
        -> m a

    {-|
    Match on a chunk of at most /n/ length, returning either the value or an error. If it fails, the parser will
    backtrack the stream. If this succeeds and chunk is non-empty, input is consumed.

    For matching by equality, use the derived 'string' combinator.
    -}
    matchTokens
        :: -- | Length /n/ of the chunk to take. If /n/ <= 0, no tokens are taken.
           Int
        -> -- | A function to match on the chunk. If the chunk is empty, then /n/ <= 0 or we are at the end.
           (Chunk s -> Either (ErrorItem s e l) a)
        -> m a

    {-|
    Take tokens that satisfy a predicate, and match on them, returning either the value or an error. If it fails,
    the parser will backtrack the stream. If this succeeds and the chunk is non-empty, input is consumed.

    For matching just by a predicate, use the derived 'tokenWhile' and 'tokenWhile1' combinators.
    -}
    matchTokenWhile
        :: -- | The predicate to check a token.
           (Token s -> Bool)
        -> -- | A function to match on the chunk.
           (Chunk s -> Either (ErrorItem s e l) a)
        -> m a

    -- | A parser that only succeeds at the end of the stream.
    endOfInput :: m ()

    {-|
    Adds or removes a label for a parser. See 'label' and 'hidden' for more information.

    By default, no parsers defined in this library are labelled. It is entirely up to you to label parsers.
    -}
    withLabel :: Maybe l -> m a -> m a

    {-|
    Backtracks a parser if it failed. That is, if a parser @p@ fails, then @try p@ will be considered to not have
    consumed input. This can be used for arbitrary lookahead.

    In the example below, @alt1@ will not act as expected, since @red@ will consume the \'r', meaning @rad@ will not
    be tried. Adding @try@ in @alt2@ will allow it to work as expected.

@
red = 'char' \'r' >> 'char' \'e' >> 'char' \'d'
rad = 'char' \'r' >> 'char' \'a' >> 'char' \'d'
alt1 = red \<|> rad
alt2 = try red \<|> rad
@
    -}
    try :: m a -> m a

    {-|
    Backtracks a parser if it succeeds. That is, if a parser @p@ succeeds, then @lookahead p@ will be considered to not
    have consumed input.

    This does not affect the parser if it fails, i.e. failed parsers can still consume input. Use 'try' along with this
    function if you need to backtrack on failure too.
    -}
    lookahead :: m a -> m a

    {-|
    Creates a parser that only succeeds if the original fails.

    This parser never consumes input nor modifies parser state.
    -}
    notFollowedBy :: m a -> m ()

    {-|
    Creates a parser that can recover from parse failures.

    If the recovery parser fails, it will act as if only the original parser failed.
    -}
    recover :: (ParseError s e l -> m a) -> m a -> m a

    {-|
    Observes the result of a parser, allowing parsing to continue on failure.

    Note that this does not backtrack the parser whether it succeeds or fails.
    -}
    observing :: m a -> m (Either (ParseError s e l) a)

    -- | Fails parsing with a parse error.
    parseError :: ParseError s e l -> m a

    -- | Gets the parser state.
    getState :: m (State s)

    -- | Replaces the parser state.
    putState :: State s -> m ()

-- | Parses any token.
anyToken :: MonadParser s e l m => m (Token s)
anyToken = matchToken $ \m ->
    case m of
        Nothing -> Left (ErrorItemLabels UnexpectedEnd [])
        Just x -> Right x
{-# INLINABLE anyToken #-}

{-|
Parses a specific token. Note that this parser is not labelled by default.

@semicolon = char \';\'@
-}
char :: (MonadParser s e l m, Eq (Token s)) => Token s -> m (Token s)
char expected = matchToken $ \m ->
    case m of
        Just t | t == expected -> Right t
        Just t -> Left (ErrorItemLabels (UnexpectedToken t) [])
        Nothing -> Left (ErrorItemLabels UnexpectedEnd [])
{-# INLINABLE char #-}

{-|
Parses a specific sequence of tokens. This fully backtracks, since it uses 'matchTokens'. Note that this parser is not
labelled by default.

@color = string \"red\" \<|> string \"green\" \<|> string \"blue\"@
-}
string :: forall s e l m. (MonadParser s e l m, Eq (Chunk s)) => Chunk s -> m (Chunk s)
string expected = matchTokens (chunkLength proxy expected) $ \ys ->
    if expected == ys
        then Right ys
        else Left (ErrorItemLabels (UnexpectedChunk ys) [])
    where
        proxy = Proxy :: Proxy s
{-# INLINABLE string #-}

{-|
Parses a token that satisfies a predicate.

@digit = satisfy isDigit@
-}
satisfy :: MonadParser s e l m => (Token s -> Bool) -> m (Token s)
satisfy p = matchToken $ \m ->
    case m of
        Just t | p t -> Right t
        Just t -> Left (ErrorItemLabels (UnexpectedToken t) [])
        Nothing -> Left (ErrorItemLabels UnexpectedEnd [])
{-# INLINABLE satisfy #-}

-- | Peeks at the next token, without advancing the stream in any way.
peek :: MonadParser s e l m => m (Maybe (Token s))
peek = fmap fst . streamUncons <$> getInput
{-# INLINABLE peek #-}

{-|
Peeks at the next token, without advancing the stream in any way. If the stream is empty (i.e. there is no next token),
an unexpected end of input error is reported.
-}
peekNext :: MonadParser s e l m => m (Token s)
peekNext = do
    m <- streamUncons <$> getInput
    case m of
        Nothing -> unexpected UnexpectedEnd []
        Just (x, _) -> pure x
{-# INLINABLE peekNext #-}

-- | Parses a chunk of length exactly /n/, not more, not less. This fully backtracks, since it uses 'matchTokens'.
countTokens :: forall s e l m. MonadParser s e l m => Int -> m (Chunk s)
countTokens n = matchTokens n $ \ys ->
    if chunkLength proxy ys == n
        then Right ys
        else Left (ErrorItemLabels (UnexpectedChunk ys) [])
    where
        proxy = Proxy :: Proxy s
{-# INLINABLE countTokens #-}

{-|
Takes zero or more tokens that match a predicate. The resulting parser cannot fail. This fully backtracks,
since it uses 'matchTokenWhile'. This should be more performant than using 'Control.Applicative.many' and 'satisfy'.

@digits = tokenWhile isDigit@
-}
tokenWhile :: MonadParser s e l m => (Token s -> Bool) -> m (Chunk s)
tokenWhile p = matchTokenWhile p Right
{-# INLINABLE tokenWhile #-}

{-|
Takes one or more tokens that match a predicate. This fully backtracks, since it uses 'matchTokenWhile'. This should
be more performant than using 'Control.Applicative.some' and 'satisfy'.

@digits1 = tokenWhile1 isDigit@
-}
tokenWhile1 :: forall s e l m. MonadParser s e l m => (Token s -> Bool) -> m (Chunk s)
tokenWhile1 p = matchTokenWhile p $ \xs ->
    if chunkNull proxy xs
        then Left (ErrorItemLabels UnexpectedEnd [])
        else Right xs
    where
        proxy = Proxy :: Proxy s
{-# INLINABLE tokenWhile1 #-}

-- | Consumes the rest of the input. This parser cannot fail, though the chunk may be empty.
matchRest :: MonadParser s e l m => m (Chunk s)
matchRest = tokenWhile (const True)
{-# INLINABLE matchRest #-}

-- | A parser that checks whether we are at the end of the stream.
atEnd :: MonadParser s e l m => m Bool
atEnd = do
    m <- streamUncons <$> getInput
    case m of
        Nothing -> pure True
        Just _ -> pure False
{-# INLINABLE atEnd #-}

{-|
Adds a label to a parser. This is used for labelling parsers that do not have one for better error messages, or for
labelling a complex combination of parsers where you want to give it a more general label instead of merging the labels
of each constituent parser.

@label lbl p = 'withLabel' (Just lbl) p@
-}
label :: MonadParser s e l m => l -> m a -> m a
label lbl p = withLabel (Just lbl) p
{-# INLINABLE label #-}

-- | Adds a label to a parser. Simply a synonym for @flip 'label'@.
infix 0 <?>
(<?>) :: MonadParser s e l m => m a -> l -> m a
p <?> lbl = label lbl p
{-# INLINABLE (<?>) #-}

{-|
Removes the label from a parser. This can be used to hide labels from errors.

@hidden p = 'withLabel' Nothing p@
-}
hidden :: MonadParser s e l m => m a -> m a
hidden p = withLabel Nothing p
{-# INLINABLE hidden #-}

{-|
Restores the state to before using the parser if the error passes a predicate.

The result parser still fails if the given parser fails.
-}
restore :: MonadParser s e l m => (ParseError s e l -> Bool) -> m a -> m a
restore f p = do
    st <- getState
    r <- observing p
    case r of
        Right x -> pure x
        Left e -> do
            when (f e) $ putState st
            parseError e
{-# INLINABLE restore #-}

-- | Fails parsing with an unexpected item and a list of expected items.
unexpected :: MonadParser s e l m => Unexpected s -> [l] -> m a
unexpected unex ls = do
    st <- getState
    parseError (makeErrorAt st (ErrorItemLabels unex ls))
{-# INLINABLE unexpected #-}

-- | Fails parsing with a failure message. These errors are generally for broken invariants.
failure :: MonadParser s e l m => String -> m a
failure msg = do
    st <- getState
    parseError (makeErrorAt st (ErrorItemMessages [MessageFail msg]))
{-# INLINABLE failure #-}

-- | Fails parsing with a custom error.
customError :: MonadParser s e l m => e -> m a
customError e = do
    st <- getState
    parseError (makeErrorAt st (ErrorItemMessages [MessageCustom e]))
{-# INLINABLE customError #-}

{-|
Gets the parser state applied to a function.

@getsState f = f \<$> 'getState'@
-}
getsState :: MonadParser s e l m => (State s -> a) -> m a
getsState f = f <$> getState
{-# INLINABLE getsState #-}

{-|
Modifies the parser state by a function.

@modifyState f = 'getState' >>= 'putState' . f@
-}
modifyState :: MonadParser s e l m => (State s -> State s) -> m ()
modifyState f = getState >>= putState . f
{-# INLINABLE modifyState #-}

-- | Gets the input.
getInput :: MonadParser s e l m => m s
getInput = getsState stateInput
{-# INLINABLE getInput #-}

{-|
Gets the input applied to a function.

@getsInput f = f \<$> 'getInput'@
-}
getsInput :: MonadParser s e l m => (s -> a) -> m a
getsInput f = getsState (f . stateInput)
{-# INLINABLE getsInput #-}

-- | Replaces the input.
putInput :: MonadParser s e l m => s -> m ()
putInput s = modifyState $ \st -> st { stateInput = s }
{-# INLINABLE putInput #-}

{-|
Modifies the input by a function.

@modifyInput f = 'getInput' >>= 'putInput' . f@
-}
modifyInput :: MonadParser s e l m => (s -> s) -> m ()
modifyInput f = getInput >>= putInput . f
{-# INLINABLE modifyInput #-}

-- | Gets the position in the source text.
getPos :: MonadParser s e l m => m Pos
getPos = getsState statePos
{-# INLINABLE getPos #-}

-- | Gets the offset in the input stream.
getOffset :: MonadParser s e l m => m Int
getOffset = getsState stateOffset
{-# INLINABLE getOffset #-}

infixr 9 .:
(.:) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(.:) = (.) . (.)
{-# INLINE (.:) #-}

infixr 9 .::
(.::) :: (b -> c) -> (a1 -> a2 -> a3 -> b) -> a1 -> a2 -> a3 -> c
(.::) = (.) . (.) . (.)
{-# INLINE (.::) #-}

instance (Monoid w, MonadParser s e l m) => MonadParser s e l (M.AccumT w m) where
    matchToken = lift . matchToken
    matchTokens = lift .: matchTokens
    matchTokenWhile = lift .: matchTokenWhile
    endOfInput = lift endOfInput
    withLabel l = M.AccumT . withLabel l .: M.runAccumT
    try = M.AccumT . try .: M.runAccumT
    lookahead = M.AccumT . lookahead .: M.runAccumT
    notFollowedBy m = M.AccumT $ \w -> fmap (, mempty) . notFollowedBy $ M.runAccumT m w
    recover f m = M.AccumT $ \w -> recover (flip M.runAccumT w . f) (M.runAccumT m w)
    observing m = M.AccumT $ \w -> fmap adjustWriterT . observing $ M.runAccumT m w
    parseError = lift . parseError
    getState = lift getState
    putState = lift . putState

instance (Monoid err, MonadParser s e l m) => MonadParser s e l (M.ExceptT err m) where
    matchToken = lift . matchToken
    matchTokens = lift .: matchTokens
    matchTokenWhile = lift .: matchTokenWhile
    endOfInput = lift endOfInput
    withLabel l = M.ExceptT . withLabel l . M.runExceptT
    try = M.ExceptT . try . M.runExceptT
    lookahead = M.ExceptT . lookahead . M.runExceptT
    notFollowedBy = M.ExceptT . fmap pure . notFollowedBy . M.runExceptT
    recover f m = M.ExceptT $ recover (M.runExceptT . f) (M.runExceptT m)
    observing = M.ExceptT . fmap sequence . observing . M.runExceptT
    parseError = lift . parseError
    getState = lift getState
    putState = lift . putState

instance MonadParser s e l m => MonadParser s e l (M.IdentityT m) where
    matchToken = lift . matchToken
    matchTokens = lift .: matchTokens
    matchTokenWhile = lift .: matchTokenWhile
    endOfInput = lift endOfInput
    withLabel l = M.IdentityT . withLabel l . M.runIdentityT
    try = M.IdentityT . try . M.runIdentityT
    lookahead = M.IdentityT . lookahead . M.runIdentityT
    notFollowedBy = M.IdentityT . notFollowedBy . M.runIdentityT
    recover f m = M.IdentityT $ recover (M.runIdentityT . f) (M.runIdentityT m)
    observing = M.IdentityT . observing . M.runIdentityT
    parseError = lift . parseError
    getState = lift getState
    putState = lift . putState

instance MonadParser s e l m => MonadParser s e l (M.MaybeT m) where
    matchToken = lift . matchToken
    matchTokens = lift .: matchTokens
    matchTokenWhile = lift .: matchTokenWhile
    endOfInput = lift endOfInput
    withLabel l = M.MaybeT . withLabel l . M.runMaybeT
    try = M.MaybeT . try . M.runMaybeT
    lookahead = M.MaybeT . lookahead . M.runMaybeT
    notFollowedBy = M.MaybeT . fmap pure . notFollowedBy . M.runMaybeT
    recover f m = M.MaybeT $ recover (M.runMaybeT . f) (M.runMaybeT m)
    observing = M.MaybeT . fmap sequence . observing . M.runMaybeT
    parseError = lift . parseError
    getState = lift getState
    putState = lift . putState

instance (Monoid w, MonadParser s e l m) => MonadParser s e l (C.RWST r w st m) where
    matchToken = lift . matchToken
    matchTokens = lift .: matchTokens
    matchTokenWhile = lift .: matchTokenWhile
    endOfInput = lift endOfInput
    withLabel l = C.rwsT . withLabel l .:: C.runRWST
    try = C.rwsT . try .:: C.runRWST
    lookahead = C.rwsT . lookahead .:: C.runRWST
    notFollowedBy m = C.rwsT $ \r st -> fmap (, st, mempty) . notFollowedBy $ C.runRWST m r st
    recover f m = C.rwsT $ \r st -> recover (\e -> C.runRWST (f e) r st) (C.runRWST m r st)
    observing m = C.rwsT $ \r st -> fmap (adjustRWST st) . observing $ C.runRWST m r st
    parseError = lift . parseError
    getState = lift getState
    putState = lift . putState

instance (Monoid w, MonadParser s e l m) => MonadParser s e l (L.RWST r w st m) where
    matchToken = lift . matchToken
    matchTokens = lift .: matchTokens
    matchTokenWhile = lift .: matchTokenWhile
    endOfInput = lift endOfInput
    withLabel l = L.RWST . withLabel l .:: L.runRWST
    try = L.RWST . try .:: L.runRWST
    lookahead = L.RWST . lookahead .:: L.runRWST
    notFollowedBy m = L.RWST $ \r st -> fmap (, st, mempty) . notFollowedBy $ L.runRWST m r st
    recover f m = L.RWST $ \r st -> recover (\e -> L.runRWST (f e) r st) (L.runRWST m r st)
    observing m = L.RWST $ \r st -> fmap (adjustRWST st) . observing $ L.runRWST m r st
    parseError = lift . parseError
    getState = lift getState
    putState = lift . putState

instance (Monoid w, MonadParser s e l m) => MonadParser s e l (S.RWST r w st m) where
    matchToken = lift . matchToken
    matchTokens = lift .: matchTokens
    matchTokenWhile = lift .: matchTokenWhile
    endOfInput = lift endOfInput
    withLabel l = S.RWST . withLabel l .:: S.runRWST
    try = S.RWST . try .:: S.runRWST
    lookahead = S.RWST . lookahead .:: S.runRWST
    notFollowedBy m = S.RWST $ \r st -> fmap (, st, mempty) . notFollowedBy $ S.runRWST m r st
    recover f m = S.RWST $ \r st -> recover (\e -> S.runRWST (f e) r st) (S.runRWST m r st)
    observing m = S.RWST $ \r st -> fmap (adjustRWST st) . observing $ S.runRWST m r st
    parseError = lift . parseError
    getState = lift getState
    putState = lift . putState

instance MonadParser s e l m => MonadParser s e l (M.ReaderT r m) where
    matchToken = lift . matchToken
    matchTokens = lift .: matchTokens
    matchTokenWhile = lift .: matchTokenWhile
    endOfInput = lift endOfInput
    withLabel l = M.ReaderT . withLabel l .: M.runReaderT
    try = M.ReaderT . try .: M.runReaderT
    lookahead = M.ReaderT . lookahead .: M.runReaderT
    notFollowedBy = M.ReaderT . notFollowedBy .: M.runReaderT
    recover f m = M.ReaderT $ \r -> recover (flip M.runReaderT r . f) (M.runReaderT m r)
    observing = M.ReaderT . observing .: M.runReaderT
    parseError = lift . parseError
    getState = lift getState
    putState = lift . putState

instance MonadParser s e l m => MonadParser s e l (L.StateT st m) where
    matchToken = lift . matchToken
    matchTokens = lift .: matchTokens
    matchTokenWhile = lift .: matchTokenWhile
    endOfInput = lift endOfInput
    withLabel l = L.StateT . withLabel l .: L.runStateT
    try = L.StateT . try .: L.runStateT
    lookahead = L.StateT . lookahead .: L.runStateT
    notFollowedBy m = L.StateT $ \st -> fmap (, st) . notFollowedBy $ L.runStateT m st
    recover f m = L.StateT $ \st -> recover (flip L.runStateT st . f) (L.runStateT m st)
    observing m = L.StateT $ \st -> fmap (adjustStateT st) . observing $ L.runStateT m st
    parseError = lift . parseError
    getState = lift getState
    putState = lift . putState

instance MonadParser s e l m => MonadParser s e l (S.StateT st m) where
    matchToken = lift . matchToken
    matchTokens = lift .: matchTokens
    matchTokenWhile = lift .: matchTokenWhile
    endOfInput = lift endOfInput
    withLabel l = S.StateT . withLabel l .: S.runStateT
    try = S.StateT . try .: S.runStateT
    lookahead = S.StateT . lookahead .: S.runStateT
    notFollowedBy m = S.StateT $ \st -> fmap (, st) . notFollowedBy $ S.runStateT m st
    recover f m = S.StateT $ \st -> recover (flip S.runStateT st . f) (S.runStateT m st)
    observing m = S.StateT $ \st -> fmap (adjustStateT st) . observing $ S.runStateT m st
    parseError = lift . parseError
    getState = lift getState
    putState = lift . putState

instance (Monoid w, MonadParser s e l m) => MonadParser s e l (C.WriterT w m) where
    matchToken = lift . matchToken
    matchTokens = lift .: matchTokens
    matchTokenWhile = lift .: matchTokenWhile
    endOfInput = lift endOfInput
    withLabel l = C.writerT . withLabel l . C.runWriterT
    try = C.writerT . try . C.runWriterT
    lookahead = C.writerT . lookahead . C.runWriterT
    notFollowedBy = C.writerT . fmap (, mempty) . notFollowedBy . C.runWriterT
    recover f m = C.writerT $ recover (C.runWriterT . f) (C.runWriterT m)
    observing = C.writerT . fmap adjustWriterT . observing . C.runWriterT
    parseError = lift . parseError
    getState = lift getState
    putState = lift . putState

instance (Monoid w, MonadParser s e l m) => MonadParser s e l (L.WriterT w m) where
    matchToken = lift . matchToken
    matchTokens = lift .: matchTokens
    matchTokenWhile = lift .: matchTokenWhile
    endOfInput = lift endOfInput
    withLabel l = L.WriterT . withLabel l . L.runWriterT
    try = L.WriterT . try . L.runWriterT
    lookahead = L.WriterT . lookahead . L.runWriterT
    notFollowedBy = L.WriterT . fmap (, mempty) . notFollowedBy . L.runWriterT
    recover f m = L.WriterT $ recover (L.runWriterT . f) (L.runWriterT m)
    observing = L.WriterT . fmap adjustWriterT . observing . L.runWriterT
    parseError = lift . parseError
    getState = lift getState
    putState = lift . putState

instance (Monoid w, MonadParser s e l m) => MonadParser s e l (S.WriterT w m) where
    matchToken = lift . matchToken
    matchTokens = lift .: matchTokens
    matchTokenWhile = lift .: matchTokenWhile
    endOfInput = lift endOfInput
    withLabel l = S.WriterT . withLabel l . S.runWriterT
    try = S.WriterT . try . S.runWriterT
    lookahead = S.WriterT . lookahead . S.runWriterT
    notFollowedBy = S.WriterT . fmap (, mempty) . notFollowedBy . S.runWriterT
    recover f m = S.WriterT $ recover (S.runWriterT . f) (S.runWriterT m)
    observing = S.WriterT . fmap adjustWriterT . observing . S.runWriterT
    parseError = lift . parseError
    getState = lift getState
    putState = lift . putState

adjustRWST :: Monoid w => st -> Either b (a, st, w) -> (Either b a, st, w)
adjustRWST st (Left b) = (Left b, st, mempty)
adjustRWST _ (Right (a, st, w)) = (Right a, st, w)
{-# INLINE adjustRWST #-}

adjustStateT :: st -> Either b (a, st) -> (Either b a, st)
adjustStateT st (Left b) = (Left b, st)
adjustStateT _ (Right (a, st)) = (Right a, st)
{-# INLINE adjustStateT #-}

adjustWriterT :: Monoid w => Either b (a, w) -> (Either b a, w)
adjustWriterT (Left b) = (Left b, mempty)
adjustWriterT (Right (a, w)) = (Right a, w)
{-# INLINE adjustWriterT #-}
