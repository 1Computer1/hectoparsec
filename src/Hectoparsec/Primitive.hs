{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-|
Module      : Hectoparsec.Primitive
Copyright   : (c) comp 2020
License     : MIT
Maintainer  : onecomputer00@gmail.com
Stability   : stable
Portability : portable

Primitive definitions and operations for parser combinators.
-}
module Hectoparsec.Primitive
    ( -- * Parser types
      ParserT(..)
    , Parser
    , ThenOk
    , ThenErr
    , Continuations
    , Reply(..)
    , makeParserT
    , contParserT
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Cont.Class
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad.RWS.Class
import Control.Monad.Trans
import Data.Functor.Identity
import Data.Proxy
import Data.String
import Hectoparsec.Class
import Hectoparsec.Error
import Hectoparsec.State
import Hectoparsec.Stream

{-|
The type of a parser for a stream @s@, using custom errors @e@ and custom labels @l@.

If custom errors or custom labels are not needed, you can simply set it to 'Data.Void.Void' to ignore it. Generally,
if your parser cannot error, you would do so. Otherwise, you should set the error and label types to something that
would allow you to create useful error messages. In particular, labels are tracked in order to create a list of
expected items whenever parsers fail.

'ParserT' implements 'MonadParser' for the primitive parser combinators, 'Alternative' for branching parsers, and the
usual stack of 'Functor', 'Applicative', and 'Monad', along with the classes from @mtl@.
-}
newtype ParserT s e l m a = ParserT { unParserT :: Continuations s e l m a }

-- | The 'ParserT' type specialized to the 'Identity' monad.
type Parser s e l = ParserT s e l Identity

-- | Continuation for a successful parse. The labels are used for tracking what was expected.
type ThenOk s l m a r = a -> [l] -> State s -> m r

-- | Continuation for a failed parse.
type ThenErr s e l m r = ParseError s e l -> State s -> m r

-- | Continuations for a parser. 'ParserT' uses this for performance reasons.
type Continuations s e l m a =
    forall r
    .  State s
    -> ThenOk s l m a r
    -> ThenErr s e l m r
    -> ThenOk s l m a r
    -> ThenErr s e l m r
    -> m r

-- | The reply after parsing.
data Reply s e l a = Reply
    { replyState    :: !(State s)                  -- ^ Resulting state.
    , replyConsumed :: Bool                        -- ^ Whether the parser consumed or not.
    , replyResult   :: Either (ParseError s e l) a -- ^ Either the error or result value.
    }

-- | Creates a parser based on a reply.
makeParserT :: Monad m => (State s -> m (Reply s e l a)) -> ParserT s e l m a
makeParserT r = ParserT $ \st cok cerr uok uerr -> do
    Reply st' sump res <- r st
    case (sump, res) of
        (True, Right a)  -> cok a [] st'
        (True, Left e)   -> cerr e st'
        (False, Right a) -> uok a [] st'
        (False, Left e)  -> uerr e st'
{-# INLINE makeParserT #-}

-- | Runs a parser and retrieve the reply.
contParserT :: Monad m => ParserT s e l m a -> State s -> m (Reply s e l a)
contParserT p s = unParserT p s cok cerr uok uerr
    where
        cok a _ st = return $ Reply st True (Right a)
        cerr e st  = return $ Reply st True (Left e)
        uok a _ st = return $ Reply st False (Right a)
        uerr e st  = return $ Reply st False (Left e)
{-# INLINE contParserT #-}

-- | Lifts the underlying 'Semigroup' into the parser.
instance Semigroup a => Semigroup (ParserT s e l m a) where
    p <> q = liftA2 (<>) p q
    {-# INLINE (<>) #-}

-- | Lifts the underlying 'Monoid' into the parser.
instance Monoid a => Monoid (ParserT s e l m a) where
    mempty = pure mempty
    {-# INLINE mempty #-}

instance Functor (ParserT s e l m) where
    fmap f p = ParserT $ \st cok cerr uok uerr ->
        unParserT p st (cok . f) cerr (uok . f) uerr
    {-# INLINE fmap #-}

instance Applicative (ParserT s e l m) where
    pure a = ParserT $ \st _ _ uok _ -> uok a [] st
    {-# INLINE pure #-}

    p <*> q = ParserT $ \st cok cerr uok uerr ->
        let pcok pa pl pst = unParserT q pst (cok . pa) cerr (withOk pl (cok . pa)) (withErr pl cerr)
            puok pa pl pst = unParserT q pst (cok . pa) cerr (withOk pl (uok . pa)) (withErr pl uerr)
        in unParserT p st pcok cerr puok uerr
    {-# INLINE (<*>) #-}

{-|
Allows for branching parsers. The 'empty' parser will always fail.

Note that @p '<|>' q@ will only try @q@ if @p@ fails and did not consume any input. For parsers @p@ that consume input,
they can be backtracked to allow the next parser to be attempted using @'try' p@.

In general, if any branch comsumes input, regardless of success, that branch will be commited to, and error messages
will be based entirely on that branch.
-}
instance Alternative (ParserT s e l m) where
    empty = ParserT $ \st _ _ _ uerr -> uerr (makeErrorAt st (ErrorItemLabels UnexpectedEmpty [])) st
    {-# INLINE empty #-}

    p <|> q = ParserT $ \st cok cerr uok uerr ->
        let puerr pe pst =
                let quok qa ql qst = uok qa (merge pe qst ql) qst
                    querr qe qst = uncurry (flip uerr) (choose (pst, pe) (qst, qe))
                in unParserT q st cok cerr quok querr
        in unParserT p st cok cerr uok puerr
    {-# INLINE (<|>) #-}

-- | Equivalent to the 'Alternative' instance.
instance MonadPlus (ParserT s e l m) where

instance Monad (ParserT s e l m) where
    p >>= q = ParserT $ \st cok cerr uok uerr ->
        let pcok pa pl pst = unParserT (q pa) pst cok cerr (withOk pl cok) (withErr pl cerr)
            puok pa pl pst = unParserT (q pa) pst cok cerr (withOk pl uok) (withErr pl uerr)
        in unParserT p st pcok cerr puok uerr
    {-# INLINE (>>=) #-}

instance MonadFail (ParserT s e l m) where
    fail msg = ParserT $ \st _ _ _ uerr -> uerr (makeErrorAt st $ ErrorItemMessages [MessageFail msg]) st
    {-# INLINE fail #-}

instance Stream s => MonadParser s e l (ParserT s e l m) where
    matchToken match = ParserT $ \st@(State {..}) cok _ uok uerr ->
        case streamUncons stateInput of
            Nothing -> case match Nothing of
                Left err -> uerr (makeErrorAt st err) st
                Right res -> uok res [] st
            Just (tok, toks) -> case match (Just tok) of
                Left err -> uerr (makeErrorAt st err) st
                Right res -> cok res []
                    (st
                        { stateInput = toks
                        , statePos = updatePosToken proxy tok statePos
                        })
        where proxy = Proxy :: Proxy s
    {-# INLINE matchToken #-}

    matchTokens n match = ParserT $ \st@(State {..}) cok _ uok uerr ->
        let (xs, ys) = streamSplitAt n stateInput
        in case match xs of
            Left err -> uerr (makeErrorAt st err) st
            Right res -> if chunkNull proxy xs
                then uok res [] st
                else cok res []
                    (st
                        { stateInput = ys
                        , statePos = updatePosChunk proxy xs statePos
                        })
        where
            proxy = Proxy :: Proxy s
    {-# INLINE matchTokens #-}

    matchTokenWhile p match = ParserT $ \st@(State {..}) cok _ uok uerr ->
        let (xs, ys) = streamSpan p stateInput
        in case match xs of
            Left err  -> uerr (makeErrorAt st err) st
            Right res -> if chunkNull proxy xs
                then uok res [] st
                else cok res []
                    (st
                        { stateInput = ys
                        , statePos = updatePosChunk proxy xs statePos
                        })
        where
            proxy = Proxy :: Proxy s
    {-# INLINE matchTokenWhile #-}

    endOfInput = ParserT $ \st@(State {..}) _ _ uok uerr ->
        case streamUncons stateInput of
            Nothing -> uok () [] st
            Just _  -> uerr (makeErrorAt st (ErrorItemLabels UnexpectedEnd [])) st
    {-# INLINE endOfInput #-}

    withLabel lbl p = ParserT $ \st@(State {..}) cok cerr uok uerr ->
        let pcok pa pl st' = case lbl of
                -- We only want to be able to remove the last label.
                -- If something was consumed, it does not make sense to replace the previous label.
                Nothing -> cok pa (replaceLabel Nothing pl) st'
                Just _  -> cok pa pl st'
            puok pa pl st' = uok pa (replaceLabel lbl pl) st'
            puerr pe = uerr . ParseError (parseErrorPos pe) (parseErrorOffset pe) $
                case parseErrorItem pe of
                    ErrorItemLabels unex _ -> ErrorItemLabels unex (maybe [] pure lbl)
                    ei -> ei
        in unParserT p st pcok cerr puok puerr
        where
            replaceLabel _ [] = []
            replaceLabel Nothing (_:xs) = xs
            replaceLabel (Just x) (_:xs) = x:xs
    {-# INLINE withLabel #-}

    try p = ParserT $ \st cok _ uok uerr ->
        let puerr pe _ = uerr pe st
        in unParserT p st cok puerr uok puerr
    {-# INLINE try #-}

    lookahead p = ParserT $ \st _ cerr uok uerr ->
        let puok pa _ _ = uok pa [] st
        in unParserT p st puok cerr puok uerr
    {-# INLINE lookahead #-}

    notFollowedBy p = ParserT $ \st _ _ uok uerr ->
        let puok _ _ _ =
                let tok = fst <$> streamUncons (stateInput st)
                in uerr (makeErrorAt st $ ErrorItemLabels (maybe UnexpectedEnd UnexpectedToken tok) []) st
            puerr _ _ = uok () [] st
        in unParserT p st puok puerr puok puerr
    {-# INLINE notFollowedBy #-}

    recover recv p = ParserT $ \st cok cerr uok uerr ->
        let pcerr pe pst =
                let rcok ra _ rst  = cok ra [] rst
                    rcerr _ _      = cerr pe pst
                    ruok ra rl rst = uok ra (merge pe rst rl) rst
                    ruerr _ _      = cerr pe pst
                in unParserT (recv pe) pst rcok rcerr ruok ruerr
            puerr pe pst =
                let rcok ra rl rst = cok ra (merge pe rst rl) rst
                    rcerr _ _      = uerr pe pst
                    ruok ra rl rst = uok ra (merge pe rst rl) rst
                    ruerr _ _      = uerr pe pst
                in unParserT (recv pe) pst rcok rcerr ruok ruerr
        in unParserT p st cok pcerr uok puerr
    {-# INLINE recover #-}

    observing p = ParserT $ \st cok _ uok _ ->
        let pcerr pe pst = cok (Left pe) [] pst
            puerr pe pst = uok (Left pe) [] pst
        in unParserT p st (cok . Right) pcerr (uok . Right) puerr
    {-# INLINE observing #-}

    parseError e = ParserT $ \st _ _ _ uerr -> uerr e st
    {-# INLINE parseError #-}

    getState = ParserT $ \st _ _ uok _ -> uok st [] st
    {-# INLINE getState #-}

    putState st = ParserT $ \_ _ _ uok _ -> uok () [] st
    {-# INLINE putState #-}

-- | Allows for overloaded string literals to become parsers. This is equivalent to calling 'string'.
instance (Stream s, IsString a, Eq a, a ~ Chunk s) => IsString (ParserT s e l m a) where
    fromString xs = string (fromString xs)
    {-# INLINE fromString #-}

-- | Merges the labels from a parse error and the labels at some state together.
merge :: ParseError s e l -> State s -> [l] -> [l]
merge pe qst ql =
        let pl = case parseErrorItem pe of
                ErrorItemLabels _ ls -> ls
                _ -> mempty
        in case compare (parseErrorOffset pe) (stateOffset qst) of
            LT -> ql
            EQ -> pl <> ql
            GT -> pl
{-# INLINE merge #-}

-- | Choose the longer matching state and merge the errors together.
choose :: (State s, ParseError s e l) -> (State s, ParseError s e l) -> (State s, ParseError s e l)
choose (pst, pe) (qst, qe) =
    case compare (statePos pst) (statePos qst) of
        GT -> (pst, pe <> qe)
        _  -> (qst, pe <> qe)
{-# INLINE choose #-}

-- | Append labels to a success continuation.
withOk :: [l] -> ThenOk s l m a r -> ThenOk s l m a r
withOk ls1 k = \a ls2 st -> k a (ls1 <> ls2) st
{-# INLINE withOk #-}

-- | Append labels to a failure continuation.
withErr :: [l] -> ThenErr s e l m r -> ThenErr s e l m r
withErr ls1 k = \e st -> case e of
    ParseError p o (ErrorItemLabels unex ls2) -> k (ParseError p o (ErrorItemLabels unex (ls1 <> ls2))) st
    _ -> k e st
{-# INLINE withErr #-}

instance MonadTrans (ParserT s e l) where
    lift m = ParserT $ \st _ _ uok _ -> m >>= \a -> uok a [] st
    {-# INLINE lift #-}

instance MonadIO m => MonadIO (ParserT s e l m) where
    liftIO m = lift (liftIO m)
    {-# INLINE liftIO #-}

instance MonadCont m => MonadCont (ParserT s e l m) where
    callCC k =
        makeParserT $ \st ->
            callCC $ \c ->
                contParserT (k $ \a -> makeParserT $ \st' -> c $ Reply st' False (Right a)) st
    {-# INLINE callCC #-}

instance MonadError err m => MonadError err (ParserT s e l m) where
    throwError e = lift (throwError e)
    {-# INLINE throwError #-}

    catchError p h =
        makeParserT $ \st ->
            catchError (contParserT p st) $ \e ->
                contParserT (h e) st
    {-# INLINE catchError #-}

instance MonadReader r m => MonadReader r (ParserT s e l m) where
    ask = lift ask
    {-# INLINE ask #-}

    local f p = makeParserT $ \st -> local f (contParserT p st)
    {-# INLINE local #-}

instance MonadState st m => MonadState st (ParserT s e l m) where
    get = lift get
    {-# INLINE get #-}

    put st = lift (put st)
    {-# INLINE put #-}

instance MonadWriter w m => MonadWriter w (ParserT s e l m) where
    writer m = lift (writer m)
    {-# INLINE writer #-}

    tell m = lift (tell m)
    {-# INLINE tell #-}

    listen p = makeParserT $ \st -> over $ contParserT p st
        where
            over m = do
                (rep, w) <- listen m
                return $! case replyResult rep of
                    Left e  -> rep { replyResult = Left e }
                    Right a -> rep { replyResult = Right (a, w) }
    {-# INLINE listen #-}

    pass p = makeParserT $ \st -> over $ contParserT p st
        where
            over m = pass $ do
                rep <- m
                return $! case replyResult rep of
                    Left e       -> (rep { replyResult = Left e }, id)
                    Right (a, f) -> (rep { replyResult = Right a }, f)
    {-# INLINE pass #-}

instance MonadRWS r w st m => MonadRWS r w st (ParserT s e l m)
