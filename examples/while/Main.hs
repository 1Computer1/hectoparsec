{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.Writer.Strict
import           Data.List (nub, sort)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL
import           Errata
import           Hectoparsec

import           Lexer
import           Parser

--------------------------------
-- Error printing with Errata --
--------------------------------

parseErrorErrata :: ParseError TokStream CustomError CustomLabel -> [Errata]
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
    ErrorItemLabels _ ls -> pure $ errataSimple
        (Just $ red "error: unexpected item")
        (blockSimple'
            fancyRedStyle
            (posFile pos)
            Nothing
            (posLine pos, posColumn pos, Nothing)
            (Just $ "expected " <> showLabels (nub $ sort ls)))
        Nothing
    ErrorItemMessages xs -> flip map xs $ \m -> case m of
        MessageCustom (ErrorConstantCondition (L (Span fp (l1, c1) (l2, c2)) _)) -> errataSimple
            (Just $ yellow "warning: constant condition")
            (blockMerged'
                fancyYellowStyle
                fp
                Nothing
                (l1, c1, Nothing)
                (l2, c2 - 1, Just $ yellow "this expression is always true")
                (Just $ yellow "this expression is always true")
                Nothing)
            Nothing
        MessageCustom (ErrorWhileLoop (L (Span fp (l1, c1) (l2, c2)) _)) -> errataSimple
            (Just $ yellow "warning: no while loops")
            (blockMerged
                fancyYellowStyle
                fp
                Nothing
                (l1, c1, c1 + 5, Nothing) -- To highlight the `while`, since we don't store its position.
                (l2, c2 - 1, c2, Just $ yellow "while loops are bad!")
                (Just $ yellow "while loops are bad!")
                Nothing)
            Nothing
        MessageCustom (ErrorArrowBrace (L (Span fp (l1, c1) (l2, c2)) t)) -> errataSimple
            (Just $ red "error: unexpected item")
            (blockMerged'
                fancyRedStyle
                fp
                Nothing
                (l1, c1, Nothing)
                (l2, c2 - 1, Just $ red "blocks are not supported in arrow functions")
                (Just $ red "blocks are not supported in arrow functions")
                (Just $ makeMessage t [LabelExpression]))
            Nothing
        MessageFail msg -> errataSimple
            (Just $ red "error: parse failure")
            (blockSimple'
                fancyRedStyle
                (posFile pos)
                Nothing
                (posLine pos, posColumn pos, Nothing)
                (Just $ T.pack msg))
            Nothing
    where
        makeMessage :: Tok -> [CustomLabel] -> T.Text
        makeMessage t ls = mconcat
            [ "unexpected ", prettyTok t, "\n"
            , "expected ", showLabels (nub $ sort ls)
            ]

        red :: T.Text -> T.Text
        red t = "\x1b[31m" <> t <> "\x1b[0m"

        yellow :: T.Text -> T.Text
        yellow t = "\x1b[33m" <> t <> "\x1b[0m"

--------------
-- Examples --
--------------

example :: FilePath -> IO ()
example fp = do
    let fp' = "./examples/while/files/" <> fp <> ".while"
    putStrLn $ "\x1b[1mparsing " <> fp' <> ":\x1b[0m"
    src <- T.readFile fp'
    case evalParser lexer fp' src of
        -- This error should not happen. See 'Lexer.Token' for why.
        Left (ParseError p _ m) -> error $ "lexer errored at " <> show p <> ": " <> show m
        Right ts -> case runWriter (evalParserT pModule fp' (TokStream ts)) of
            (Right x, warns) -> do
                print x
                unless (null warns) $ do
                    putStrLn ""
                    TL.putStrLn $ prettyErrors src (parseErrorErrata =<< warns)
            (Left pe, warns) -> do
                TL.putStrLn $ prettyErrors src (parseErrorErrata =<< (pe:warns))
    putStrLn ""

main :: IO ()
main = do
    example "1-stuff"
    example "2-while"
    example "3-call"
    example "4-tuples"
    example "5-unknown"
    example "6-elseif"
    example "7-mess"
    example "8-arrow"
    example "9-strange"
    example "10-end"
    example "11-block"
    example "12-assign"
