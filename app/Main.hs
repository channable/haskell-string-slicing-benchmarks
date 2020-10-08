{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Lib

import Data.Text (Text)

import qualified Data.Text.IO as T

main :: IO ()
main = do
    T.putStrLn $ Lib.naiveSlice         2 11 exampleText
    T.putStrLn $ Lib.sliceWithRule      2 11 exampleText
    T.putStrLn $ Lib.sequencedSlice     2 11 exampleText
    T.putStrLn $ Lib.noInlineTakeSlice  2 11 exampleText
    -- Since koala emoji take up two Word16s in the UTF-16 representation, and
    -- reimplementedSlice does not properly count characters, the following
    -- will give a different output than the other functions:
    T.putStrLn $ Lib.reimplementedSlice 2 11 exampleText

exampleText :: Text
exampleText = "🐨🐨Hello World🐨🐨"
