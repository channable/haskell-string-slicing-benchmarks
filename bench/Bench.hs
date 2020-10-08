{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Lib

import Criterion.Main (Benchmark, bench, bgroup, defaultMain, nf)
import Data.Text (Text)

import qualified Data.Text as T

main :: IO ()
main =
    defaultMain
      [ bgroup "long-0-1000"    $ mkBenchmark 0 1000
      , bgroup "long-5000-1000" $ mkBenchmark 5000 1000
      , bgroup "long-all"       $ mkBenchmark 0 10000
      ]

{-# NOINLINE mkBenchmark #-}
mkBenchmark :: Int -> Int -> [Benchmark]
mkBenchmark start end =
    [ bench "naiveSlice"         $ nf (Lib.naiveSlice         start end) longstring
    , bench "sliceWithRule"      $ nf (Lib.sliceWithRule      start end) longstring
    , bench "sequencedSlice"     $ nf (Lib.sequencedSlice     start end) longstring
    , bench "noInlineTakeSlice"  $ nf (Lib.noInlineTakeSlice  start end) longstring
    , bench "reimplementedSlice" $ nf (Lib.reimplementedSlice start end) longstring
    ]

longstring :: Text
longstring = T.replicate 1000 "1234567890"
