-- Output simple list of fired rewrite rules
-- {-# OPTIONS_GHC -ddump-rule-firings #-}

-- Output fired rewrite rules, along with before/after comparison
-- {-# OPTIONS_GHC -ddump-rule-rewrites #-}

module Lib.NaiveSlice ( naiveSlice ) where

import Data.Text (Text)

import qualified Data.Text as T

-- Inlining is explicitly disabled for the benchmark for several reasons:
-- - Inlining is recommended against when benchmarking (see
--   https://gitlab.haskell.org/ghc/ghc/-/issues/16556)
-- - The Lib.SliceWithRule module has a rewrite rule which would be applied to
--   this function when inlined in Bench.hs
-- - Ghc creates two versions of the function, the regular non-inline version
--   and a template for inlining. This can be confusing with
--   `-ddump-rule-firings` and `-ddump-rule-rewrites`, as some rules will be
--   fired twice.
{-# NOINLINE naiveSlice #-}
naiveSlice :: Int -> Int -> Text -> Text
naiveSlice offset len = T.take len . T.drop offset


{-

Output of -ddump-rule-firings (relevant part).

  Rule fired: TEXT drop -> fused (Data.Text)
  Rule fired: TEXT take -> fused (Data.Text)
  Rule fired:
      STREAM stream/unstream fusion (Data.Text.Internal.Fusion)

Which translates the function as follows (simplified):

  naiveSlice offset len
    = \t -> take len (drop offset t)
    --                ^^^^^^^^^^^^^
    -- Applying rule "TEXT drop -> fused"
    ~ \t -> take len (unstream (S.drop offset (stream t)))
    --      ^^^^^^^^^^
    -- Applying rule "TEXT take -> fused"
    ~ \t -> unstream (S.take len (stream (unstream (S.drop offset (stream t)))))
    --                            ^^^^^^^^^^^^^^^^^^
    -- Applying rule "STREAM stream/unstream fusion"
    ~ \t -> unstream (S.take len (S.drop offset (stream t)))
-}
