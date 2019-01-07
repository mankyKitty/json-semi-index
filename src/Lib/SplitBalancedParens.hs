{-# LANGUAGE OverloadedStrings #-}
module Lib.SplitBalancedParens
  ( buildJsonSemiIndex
  , findClose
  ) where

import           Data.Vector.Storable  (Vector)
import           GHC.Word

import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BS8
import           Data.Maybe            (Maybe (..))

import           Lib.Common            (getPos)

buildJsonSemiIndex :: ByteString -> ([Int], [Int], [Int])
buildJsonSemiIndex "" = (mempty, mempty, mempty)
buildJsonSemiIndex bs = (p, bpixs, bpix)
  where
    (p, bpixs, bpix, _, _) = BS8.foldr f (mempty, mempty, mempty, False, 0) bs

    f c (pos, bpA, bpB, inStr, strLen)
      | inStr    && c == '"'  = (replicate strLen 0 <> pos, bpA, bpB, False, 0)
      | inStr    && c /= '"'  = (0:pos,   bpA,   bpB, inStr, strLen + 1)
      | c == '{' || c == '['  = (1:pos, 1:bpA, 1:bpB, inStr, strLen)
      | c == ']' || c == '}'  = (1:pos, 0:bpA, 0:bpB, inStr, strLen)
      | c == ':' || c == ','  = (1:pos, 0:bpA, 1:bpB, inStr, strLen)
      | c == '"'              = (0:pos,   bpA,   bpB, True, 0)
      | otherwise             = (0:pos,   bpA,   bpB, inStr, strLen)

findClose :: Int -> Vector Word64 -> ([Int], [Int]) -> Maybe Int
findClose r positions (bpA,bpB)
  | r == 0                                        = Nothing
  | bpA !! (op - 2) /= 1 &&  bpB !! (op - 2) /= 1 = Nothing
  | otherwise                                     = go' (0::Int) (drop (op - 1) bpA) (drop (op - 1) bpB) op
  where
    go' 0 (0:_) (0:_) cur = Just cur
    go' s (0:a) (1:b) cur = go' s a b (cur + 2)
    go' s (0:a) (0:b) cur = go' (s - 1) a b (cur + 2)
    go' s (1:a) (1:b) cur = go' (s + 1) a b (cur + 2)
    go' _ _     _     _   = Nothing

    op = getPos (r + (r `mod` 2)) positions
