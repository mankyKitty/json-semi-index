{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module Lib.WordVector
  ( BPSemiIndex (..)
  , HasBPSemiIndex (..)
  , buildJsonSemiIndex
  , findClose
  , getFinalChar
  , emptyBV
  ) where

import Debug.Trace (traceShowId)

import           Control.Lens                              (Index, IxValue,
                                                            Ixed, ix,
                                                            makeClassy, (%~))
import qualified Control.Lens                              as L
import           Data.Bits.Lens                            (bitAt)

import           Data.Vector.Storable                      (Vector)
import qualified Data.Vector.Storable                      as V
import           GHC.Word                                  (Word64)

import           Data.Bits                                 (Bits, bit)

import           Data.ByteString                           (ByteString)
import qualified Data.ByteString.Char8                     as BS8
import           Data.Maybe                                (Maybe (..),
                                                            fromMaybe)

import           HaskellWorks.Data.Bits.BitShow            (bitShow)
import           HaskellWorks.Data.Bits.BitWise            (BitWise, (.|.))
import qualified HaskellWorks.Data.RankSelect.Base.Select1 as S1

import           Lib.Common                                (getPos)

data BPSemiIndex = BPSemiIndex
  { _bpSIPositions :: Vector Word64
  , _bpSIUpperBP   :: Vector Word64
  , _bpSILowerBP   :: Vector Word64
  } deriving Eq
makeClassy ''BPSemiIndex

instance Show BPSemiIndex where
  show (BPSemiIndex {..}) = unlines
    [ "BPSemiIndex {"
    , "  _bpSIPositions = " <> bitShow _bpSIPositions
    , "  _bpSIUpperBP   = " <> bitShow _bpSIUpperBP
    , "  _bpSILowerBP   = " <> bitShow _bpSILowerBP
    , "}"
    ]

emptyBPSemiIndex :: BPSemiIndex
emptyBPSemiIndex = BPSemiIndex mempty mempty mempty

wordSize :: Int
wordSize = 64

setBit :: (Ixed b, BitWise (IxValue b), Bits (IxValue b), Index b ~ Int) => Int -> b -> b
setBit nth = ix (nth `div` wordSize) %~ (bit (nth `mod` wordSize) .|.)
{-# INLINE setBit #-}

(.?.) :: (Ixed b, BitWise (IxValue b), Bits (IxValue b), Index b ~ Int) => Int -> b -> Bool
(.?.) nth bv = fromMaybe False $ bv L.^? ix (nth `div` wordSize) . bitAt (nth `mod` wordSize)
{-# INLINE (.?.) #-}

(.!?.) :: (Ixed b, BitWise (IxValue b), Bits (IxValue b), Index b ~ Int) => Int -> b -> Bool
(.!?.) n v = not (n .?. v)
{-# INLINE (.!?.) #-}

emptyBV :: ByteString -> Vector Word64
emptyBV bs = V.replicate (1 + (BS8.length bs `div` wordSize)) 0

buildJsonSemiIndex :: ByteString -> BPSemiIndex
buildJsonSemiIndex "" = emptyBPSemiIndex
buildJsonSemiIndex bs = mk . L.view L._1 $ L.ifoldl f ((ebv, ebv, ebv), False, 0) (BS8.unpack bs)
  where
    ebv = emptyBV bs
    mk (a,b,c) = BPSemiIndex a b c

    f i ((pos, bpA, bpB), inStr, bpn) c
      | inStr    && c == '"' = ((         pos,            bpA,            bpB), False, bpn)
      | inStr    && c /= '"' = ((         pos,            bpA,            bpB), inStr, bpn)
      | c == '{' || c == '[' = ((setBit i pos, setBit bpn bpA, setBit bpn bpB), inStr, bpn + 1)
      | c == ']' || c == '}' = ((setBit i pos,            bpA,            bpB), inStr, bpn + 1)
      | c == ':' || c == ',' = ((setBit i pos,            bpA, setBit bpn bpB), inStr, bpn + 1)
      | c == '"'             = ((         pos,            bpA,            bpB), True , bpn)
      | otherwise            = ((         pos,            bpA,            bpB), inStr, bpn)

findClose :: Int -> BPSemiIndex -> Maybe Int
findClose r (BPSemiIndex {..})
  | r == 0                                                   = Nothing
  | (op - 2) .!?. _bpSIUpperBP || (op - 2) .!?. _bpSILowerBP = Nothing
  | otherwise                                                = go' (0::Word) _bpSIUpperBP _bpSILowerBP (op - 1)
  where
    go' s ba bb cur
      | s == 0 && cur .!?. ba && cur .!?. bb = Just cur
      |           cur .!?. ba && cur .?. bb  = go' s       ba bb (cur + 1)
      |           cur .!?. ba && cur .!?. bb = go' (s - 1) ba bb (cur + 1)
      |           cur .?. ba  && cur .?. bb  = go' (s + 1) ba bb (cur + 1)
      | otherwise                            = Nothing

    op = getPos (r + (r `mod` 2)) _bpSIPositions

getFinalChar :: BPSemiIndex -> ByteString -> Int -> Maybe Char
getFinalChar bp@(BPSemiIndex {..}) inp rnk = BS8.index inp
  . fromIntegral -- from Count
  . S1.select1 _bpSIPositions
  . fromIntegral
  <$> findClose rnk bp
