-- Output simple list of fired rewrite rules
-- {-# OPTIONS_GHC -ddump-rule-firings #-}

-- Output fired rewrite rules, along with before/after comparison
-- {-# OPTIONS_GHC -ddump-rule-rewrites #-}

{-# OPTIONS_GHC -Wno-orphans #-} -- This is used for the rule defined below

module Lib.SliceWithRule ( sliceWithRule ) where

import Data.Text (Text)

import qualified Data.Text as T
import qualified Data.Text.Internal.Fusion as S
import qualified Data.Text.Internal.Fusion.Common as S

-- Inlining disabled for benchmarking purposes
{-# NOINLINE sliceWithRule #-}
sliceWithRule :: Int -> Int -> Text -> Text
sliceWithRule offset len = T.take len . T.drop offset

{-# RULES
"TEXT take . drop -> unfused" [1] forall len off t.
    S.unstream (S.take len (S.drop off (S.stream t))) = T.take len (T.drop off t)
  #-}


{-

Output of -ddump-rule-firings (relevant part).

  Rule fired: TEXT drop -> fused (Data.Text)
  Rule fired: TEXT take -> fused (Data.Text)
  Rule fired:
      STREAM stream/unstream fusion (Data.Text.Internal.Fusion)
  Rule fired: TEXT take . drop -> unfused (Lib.SliceWithRule)

Which translates the function as follows (simplified):

  naiveSlice offset len
    = \t -> take len (drop offset t)
    --                ^^^^^^^^^^^^^
    -- Applying rule "TEXT drop -> fused"
    ~ \t -> take len (S.unstream (S.drop offset (S.stream t)))
    --      ^^^^^^^^^^
    -- Applying rule "TEXT take -> fused"
    ~ \t -> unstream (S.take len (S.stream (unstream (S.drop offset (S.stream t)))))
    --                            ^^^^^^^^^^^^^^^^^^^^
    -- Applying rule "STREAM stream/unstream fusion"
    ~ \t -> S.unstream (S.take len (S.drop offset (S.stream t)))
    --      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    -- Applying rule "TEXT take . drop -> unfused"
    ~ \t -> take len (drop offset t)
-}

