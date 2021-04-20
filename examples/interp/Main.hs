{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.List (nub, sort)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL
import           Data.Void
import           Errata
import           Hectoparsec
import           Text.Pretty.Simple

import           Lexer
import           Parser

--------------------------------
-- Error printing with Errata --
--------------------------------

parseErrorErrata :: ParseError LexStream Void CustomLabel -> [Errata]
parseErrorErrata (ParseError pos _ ei) = case ei of
    ErrorItemLabels (UnexpectedToken (L (Span fp (l1, c1) (l2, c2)) t)) ls -> pure $ errataSimple
        (Just $ red "error: unexpected item")
        (blockMerged'
            fancyRedStyle
            fp
            Nothing
            (l1, c1, Nothing)
            (l2, c2 - 1, Nothing)
            Nothing
            (Just $ makeMessage t ls))
        Nothing
    -- There are other unexpected items that we don't use, so we won't make those messages too pretty.
    ErrorItemLabels u ls -> pure $ errataSimple
        (Just $ red "error: unexpected item")
        (blockSimple'
            fancyRedStyle
            (posFile pos)
            Nothing
            (posLine pos, posColumn pos, Nothing)
            (Just $ "unexpected " <> T.pack (show u) <> "\nexpected " <> showLabels (nub $ sort ls)))
        Nothing
    ErrorItemMessages xs -> flip map xs $ \m -> case m of
        MessageFail msg -> errataSimple
            (Just $ red "error: parse failure")
            (blockSimple'
                fancyRedStyle
                (posFile pos)
                Nothing
                (posLine pos, posColumn pos, Nothing)
                (Just $ T.pack msg))
            Nothing
        MessageCustom e -> absurd e
    where
        makeMessage :: Tok -> [CustomLabel] -> T.Text
        makeMessage t ls = mconcat
            [ "unexpected ", prettyTok t, "\n"
            , "expected ", showLabels (nub $ sort ls)
            ]

        red :: T.Text -> T.Text
        red t = "\x1b[31m" <> t <> "\x1b[0m"

--------------
-- Examples --
--------------

example :: FilePath -> IO ()
example fp = do
    let fp' = "./examples/interp/files/" <> fp <> ".interp"
    putStrLn $ "\x1b[1mparsing " <> fp' <> ":\x1b[0m"
    src <- T.readFile fp'
    let ts = LexStream (initialState fp' src) [ModeExpr]
    case evalParser pExprTop fp' ts of
        Right x -> pPrint x
        Left pe -> TL.putStrLn $ prettyErrors src (parseErrorErrata pe)
    putStrLn ""

main :: IO ()
main = do
    example "1-add"
    example "2-str"
    example "3-both"
    example "4-nested"
    example "5-unmatched"
    example "6-parens"
    example "7-empty"
    example "8-spaces"
