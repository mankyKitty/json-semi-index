{-# LANGUAGE OverloadedStrings #-}
module Lib.Simple
  ( buildJsonSemiIndex
  , findClose
  ) where

import           GHC.Word              (Word64)

import qualified Control.Lens          as L

import           Data.Vector.Storable  (Vector)

import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BS8
import           Data.Maybe            (Maybe (..))

import           Lib.Common            (getPos)

buildJsonSemiIndex :: ByteString -> ([Int], String)
buildJsonSemiIndex "" = (mempty, mempty)
buildJsonSemiIndex bs = L.view L._1 $ BS8.foldr f ((mempty, mempty), False, 0) bs
  where
    f c ((pos, bp), inStr, strLen)
      | inStr    && c /= '"'  = ((0:pos, bp), inStr, strLen + 1)
      | inStr    && c == '"'  = ((replicate strLen 0 <> pos, bp), False, 0)
      | c == '{' || c == '['  = ((1:pos, "((" <> bp), inStr, strLen)
      | c == ']' || c == '}'  = ((1:pos, "))" <> bp), inStr, strLen)
      | c == ':' || c == ','  = ((1:pos, ")(" <> bp), inStr, strLen)
      | c == '"'              = ((0:pos, bp), True, 0)
      | otherwise             = ((0:pos, bp), inStr, strLen)

findClose :: Int -> Vector Word64 -> String -> Maybe Int
findClose r positions bp
  | r == 0                                    = Nothing
  | [bp !! (op - 2), bp !! (op - 1)] /= "(("  = Nothing
  | otherwise                                 = go' (0::Int) (drop op bp) op
  where
    go' 0   (')':')':_ ) cur = Just cur
    go' stk (')':'(':xs) cur = go' stk xs (cur + 2)
    go' stk (')':')':xs) cur = go' (stk - 1) xs (cur + 2)
    go' stk ('(':'(':xs) cur = go' (stk + 1) xs (cur + 2)
    go' _   _            _   = Nothing

    op = getPos (r + (r `mod` 2)) positions
