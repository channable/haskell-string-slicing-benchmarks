-- Output simple list of fired rewrite rules
-- {-# OPTIONS_GHC -ddump-rule-firings #-}

-- Output fired rewrite rules, along with before/after comparison
-- {-# OPTIONS_GHC -ddump-rule-rewrites #-}

module Lib.NoInlineTakeSlice ( noInlineTakeSlice ) where

import Data.Text (Text)

import qualified Data.Text as T

-- Inlining disabled for benchmarking purposes
{-# NOINLINE noInlineTakeSlice #-}
noInlineTakeSlice :: Int -> Int -> Text -> Text
noInlineTakeSlice offset len = noInlineTake len . T.drop offset

-- Disable inlining for Text's take
{-# NOINLINE noInlineTake #-}
noInlineTake :: Int -> Text -> Text
noInlineTake = T.take


{-

Output of -ddump-rule-firings (relevant part).

  Rule fired: TEXT drop -> fused (Data.Text)
  Rule fired: TEXT drop -> unfused (Data.Text)

  Note that the drop is first fused then unfused again. This means that
  streams are never built. No rules are fired on the `take` function.

-}
