module Lib.Common
  ( getPos
  ) where

import           Data.Vector.Storable                      (Vector)
import           GHC.Word                                  (Word64)

import qualified HaskellWorks.Data.RankSelect.Base.Select1 as S1

getPos :: Int -> Vector Word64 -> Int
getPos n positions = fromIntegral $ pos + (pos `mod` 2)
  where pos = S1.select1 positions (fromIntegral $ n `div` 2)
