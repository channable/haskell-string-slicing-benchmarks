-- Output simple list of fired rewrite rules
-- {-# OPTIONS_GHC -ddump-rule-firings #-}

-- Output fired rewrite rules, along with before/after comparison
-- {-# OPTIONS_GHC -ddump-rule-rewrites #-}

{-# LANGUAGE BangPatterns #-}
module Lib.SequencedSlice ( sequencedSlice ) where

import Data.Text (Text)

import qualified Data.Text as T

-- Inlining disabled for benchmarking purposes
{-# NOINLINE sequencedSlice #-}
sequencedSlice :: Int -> Int -> Text -> Text
sequencedSlice offset limit text =
  let
    !suffix = T.drop offset text
  in
    T.take limit suffix

{-
Output of -ddump-rule-firings (relevant part).
  Rule fired: TEXT drop -> fused (Data.Text)
  Rule fired: TEXT take -> fused (Data.Text)
  Rule fired: TEXT drop -> unfused (Data.Text)
  Rule fired: TEXT take -> unfused (Data.Text)
-}
