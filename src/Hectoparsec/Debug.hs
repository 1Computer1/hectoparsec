{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

{-|
Module      : Hectoparsec.Debug
Copyright   : (c) comp 2020
License     : MIT
Maintainer  : onecomputer00@gmail.com
Stability   : stable
Portability : portable

Combinators and functions for debugging 'ParserT'.
-}
module Hectoparsec.Debug
    ( -- * Re-exports
      module Debug.Trace
      -- * Debug combinators
    , pdbg
    , (<??>)
      -- * Debug printers
    , parseTest
    , fmtP
    , fmtE
    ) where

import Control.Monad.Identity
import Debug.Trace
import Hectoparsec.Error
import Hectoparsec.Primitive
import Hectoparsec.State
import Hectoparsec.Stream

{-|
Wraps a parser with 'trace' for debugging, showing the parser state before and after the parser.

A parser can either consume or not consume tokens, and it can succeed with a parsed value or fail with an error.
-}
pdbg
    :: (Stream s, Show (Token s), Show (Chunk s), Show e, Show l, Show a)
    => String
    -> ParserT s e l m a
    -> ParserT s e l m a
pdbg lbl p = ParserT $ \st cok cerr uok uerr ->
    let pcok pa ls pst = flip trace (cok pa ls pst) . wrapState st pst . concat $
            [ "  \x1b[32mconsumed ok:\x1b[0m\n"
            , "    \x1b[90mvalue:\x1b[0m ", show pa, "\n"
            , "    \x1b[90mlabels:\x1b[0m ", show ls
            ]
        pcerr pe pst  = flip trace (cerr pe pst) . wrapState st pst . concat $
            [ "  \x1b[31mconsumed err:\x1b[0m\n"
            , showError pe
            ]
        puok pa ls pst = flip trace (uok pa ls pst) . wrapState st pst . concat $
            [ "  \x1b[32munconsumed ok:\x1b[0m\n"
            , "    \x1b[90mvalue:\x1b[0m ", show pa, "\n"
            , "    \x1b[90mlabels:\x1b[0m ", show ls
            ]
        puerr pe pst  = flip trace (uerr pe pst) . wrapState st pst . concat $
            [ "  \x1b[31munconsumed err:\x1b[0m\n"
            , showError pe
            ]
    in unParserT p st pcok pcerr puok puerr
    where
        wrapState st st' xs = concat
            [ header, "\n"
            , showState "before" st, "\n"
            , xs, "\n"
            , showState "after" st'
            ]

        header = concat
            [ "\x1b[1mparser ", lbl, ":\x1b[0m"
            ]

        showState x st = concat
            [ "  \x1b[33m", x, " state:\x1b[0m\n"
            , "    \x1b[90moffset:\x1b[0m ", show $ statePos st, "\n"
            , "    \x1b[90mposition:\x1b[0m ", show $ statePos st, "\n"
            , "    \x1b[90minput:\x1b[0m ", takeSome $ stateInput st
            ]

        showError e = concat
            [ "    \x1b[90moffset:\x1b[0m ", show $ parseErrorOffset e, "\n"
            , "    \x1b[90mposition:\x1b[0m ", show $ parseErrorPos e, "\n"
            , "    \x1b[90merror:\x1b[0m ", show $ parseErrorItem e
            ]

        takeSome = show . fst . streamSplitAt 10

-- | Infix version of @flip 'pdbg'@.
infix 0 <??>
(<??>) :: (Stream s, Show (Token s), Show (Chunk s), Show e, Show l, Show a)
    => ParserT s e l m a
    -> String
    -> ParserT s e l m a
(<??>) = flip pdbg

-- | Runs a parser and prints the output or error to the console.
parseTest
    :: (Show (Token s), Show (Chunk s), Show e, Show l, Show a)
    => Parser s e l a
    -> s
    -> IO ()
parseTest p s = do
    let Reply _ _ res = runIdentity $ contParserT p (initialState "<debug>" s)
    putStrLn $ fmtP res

-- | Formats the result of running a parser for debugging purposes.
fmtP
    :: (Show (Token s), Show (Chunk s), Show e, Show l, Show a)
    => Either (ParseError s e l) a
    -> String
fmtP res = case res of
    Right x -> concat
        [ "\x1b[1;32msuccess:\x1b[0m\n"
        , "  \x1b[90mvalue:\x1b[0m ", show x
        ]
    Left e -> fmtE e

-- | Formats the errors from a parser for debugging purposes.
fmtE
    :: (Show (Token s), Show (Chunk s), Show e, Show l)
    => ParseError s e l
    -> String
fmtE e = fullmsg
    where
        fullmsg = concat
            [ "\x1b[1;31mfailure:\x1b[0m\n"
            , emsg e
            ]

        emsg (ParseError {..}) = concat
            [ "  \x1b[90mparse error:\x1b[0m\n"
            , "    \x1b[90moffset:\x1b[0m ", show parseErrorOffset, "\n"
            , "    \x1b[90mposition:\x1b[0m ", show parseErrorPos, "\n"
            , eimsg parseErrorItem
            ]

        eimsg ei = case ei of
            ErrorItemLabels unex ls -> concat
                [ "    \x1b[90munexpected:\x1b[0m ", show unex, "\n"
                , "    \x1b[90mexpected:\x1b[0m ", show ls
                ]
            ErrorItemCustom c -> concat
                [ "    \x1b[90mcustom:\x1b[0m ", show c
                ]
            ErrorItemFail msg -> concat
                [ "    \x1b[90mfail:\x1b[0m ", msg
                ]
