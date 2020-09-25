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

parseErrorErrata :: ParseError LexStream Void CustomLabel -> Errata
parseErrorErrata (ParseError pos _ ei) =
    case ei of
        ErrorItemLabels (UnexpectedToken (L (Span fp (l1, c1) (l2, c2)) t)) ls -> errataSimple
            (Just $ red "error: unexpected item")
            (blockMerged'
                fancyRedStyle
                fp
                (l1, c1, Nothing)
                (l2, c2 - 1, Nothing)
                Nothing
                (Just $ makeMessage t ls))
            Nothing
        -- There are other unexpected items, but we won't make those messages too pretty, for the sake of brevity.
        ErrorItemLabels u ls -> errataSimple
            (Just $ red "error: unexpected item")
            (blockSimple'
                fancyRedStyle
                (posFile pos)
                (posLine pos)
                (posColumn pos)
                Nothing
                (Just $ "unexpected " <> T.pack (show u) <> "\nexpected " <> showLabels (nub $ sort ls)))
            Nothing
        ErrorItemFail msg -> errataSimple
            (Just $ red "error: parse failure")
            (blockSimple'
                fancyRedStyle
                (posFile pos)
                (posLine pos)
                (posColumn pos)
                Nothing
                (Just $ T.pack msg))
            Nothing
    where
        makeMessage :: Tok -> [CustomLabel] -> T.Text
        makeMessage t ls = mconcat
            [ "unexpected ", prettyTok t, "\n"
            , "expected ", showLabels (nub $ sort ls)
            ]

        showLabels :: [CustomLabel] -> T.Text
        showLabels [] = "nothing"
        showLabels [x] = showLabel x
        showLabels [x, y] = showLabel x <> " or " <> showLabel y
        showLabels xs = T.intercalate ", " (map showLabel (init xs)) <> ", or " <> showLabel (last xs)

        showLabel :: CustomLabel -> T.Text
        showLabel h = case h of
            LabelTok t -> prettyTok t
            LabelExpression -> "an expression"
            LabelInt -> "an integer"
            LabelStr -> "a string"

        prettyTok :: Tok -> T.Text
        prettyTok t = case t of
            TokInt n -> T.pack (show n)
            TokStr x -> "string \"" <> x <> "\""
            TokLInterp -> "`$(`"
            TokLParen -> "`(`"
            TokRParen -> "`)`"
            TokQuote -> "`\"`"
            TokPlus -> "`+`"
            TokEof -> "end of input"
            TokEofComment -> "unterminated block comment"
            TokUnknown c -> "`" <> T.singleton c <> "`"

        red :: T.Text -> T.Text
        red t = "\x1b[31m" <> t <> "\x1b[0m"

--------------
-- Examples --
--------------

example :: FilePath -> IO ()
example fp = do
    let fp' = "./examples/csg/files/" <> fp <> ".csg"
    putStrLn $ "\x1b[1mparsing " <> fp' <> ":\x1b[0m"
    src <- T.readFile fp'
    let ts = LexStream (initialState fp' src) [ModeExpr]
    case evalParser pExprTop fp' ts of
        Right x -> pPrint x
        Left pe -> TL.putStrLn $ prettyErrors src [parseErrorErrata pe]
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
