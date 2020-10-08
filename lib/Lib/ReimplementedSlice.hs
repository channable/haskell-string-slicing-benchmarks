-- Note: This particular implementation does not use any functions that are
-- subject to fusion. As such you will not see text related rules being fired
-- when enabling the options below.

-- Output simple list of fired rewrite rules
-- {-# OPTIONS_GHC -ddump-rule-firings #-}

-- Output fired rewrite rules, along with before/after comparison
-- {-# OPTIONS_GHC -ddump-rule-rewrites #-}
{-# LANGUAGE BangPatterns #-}
module Lib.ReimplementedSlice ( reimplementedSlice ) where

import Data.Text (Text)

import qualified Data.Text.Array as Text.Array
import qualified Data.Text.Internal as Text.Internal
import qualified Data.Text.Unsafe as Text.Unsafe

-- Inlining disabled for benchmarking purposes
{-# NOINLINE reimplementedSlice #-}
reimplementedSlice :: Int -> Int -> Text -> Text
reimplementedSlice offset len t@(Text.Internal.Text u16data off prevLen)
    | offset2 >= prevLen = Text.Internal.empty
    | len2 <= 0          = Text.Internal.empty
    | otherwise          = Text.Unsafe.takeWord16 len2 $ Text.Unsafe.dropWord16 offset2 t
  where
    offset1 = min prevLen $ max 0 offset
    len1 = min (prevLen - offset1) $ max 0 len

    offset2 = if isHighSurrogate offset1 then offset1 + 1 else offset1
    len2 = if isLowSurrogate (offset2 + len1 - 1) then len1 - 1 else len1

    -- | Return whether the code unit at the given index ends a surrogate pair.
    -- Such a code unit must be preceded by a low surrogate in valid UTF-16.
    isHighSurrogate :: Int -> Bool
    isHighSurrogate !i =
      let
        w = Text.Array.unsafeIndex u16data (off + i)
      in
        i >= 0 && i < prevLen && w >= 0xdc00 && w <= 0xdfff

    -- | Return whether the code unit at the given index starts a surrogate pair.
    -- Such a code unit must be followed by a high surrogate in valid UTF-16.
    isLowSurrogate :: Int -> Bool
    isLowSurrogate !i =
      let
        w = Text.Array.unsafeIndex u16data (off + i)
      in
        i >= 0 && i < prevLen && w >= 0xd800 && w <= 0xdbff
