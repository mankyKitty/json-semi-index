{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Lib where

import           Control.Lens                              (Index, IxValue,
                                                            Ixed, ix, (%~))
import qualified Control.Lens                              as L
import           Data.Bits.Lens                            (bitAt)

import           Data.Vector.Storable                      (Vector)
import qualified Data.Vector.Storable                      as V
import           GHC.Word

import           Data.Bits                                 (Bits, bit)

import           Data.ByteString                           (ByteString)
import qualified Data.ByteString.Char8                     as BS8
import           Data.Maybe                                (Maybe (..),
                                                            fromMaybe)
import           HaskellWorks.Data.Bits.BitShow
import           HaskellWorks.Data.Bits.BitWise            (BitWise, (.|.))
import qualified HaskellWorks.Data.RankSelect.Base.Select1 as S1

bshow :: Show a => a -> BS8.ByteString
bshow = BS8.pack . show

getPos :: Int -> Vector Word64 -> Int
getPos n positions = fromIntegral $ pos + (pos `mod` 2)
  where pos = S1.select1 positions (fromIntegral $ n `div` 2)

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

maybeClose :: Int -> Vector Word64 -> String -> Maybe Int
maybeClose r positions bp
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

buildJsonSemiIndex2 :: ByteString -> ([Int], [Int], [Int])
buildJsonSemiIndex2 "" = (mempty, mempty, mempty)
buildJsonSemiIndex2 bs = (p, bpixs, bpix)
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

maybeClose2 :: Int -> Vector Word64 -> ([Int], [Int]) -> Maybe Int
maybeClose2 r positions (bpA,bpB)
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

setBit :: (Ixed b, BitWise (IxValue b), Bits (IxValue b), Index b ~ Int) => Int -> b -> b
setBit nth = ix (nth `div` 64) %~ (bit (nth `mod` 64) .|.)
{-# INLINE setBit #-}

(.?.) :: (Ixed b, BitWise (IxValue b), Bits (IxValue b), Index b ~ Int) => Int -> b -> Bool
(.?.) nth bv = fromMaybe False $ bv L.^? ix (nth `div` 64) . bitAt (nth `mod` 64)
{-# INLINE (.?.) #-}

(.!?.) :: (Ixed b, BitWise (IxValue b), Bits (IxValue b), Index b ~ Int) => Int -> b -> Bool
(.!?.) n v = not (n .?. v)
{-# INLINE (.!?.) #-}

emptyBV :: ByteString -> Vector Word64
emptyBV bs = V.replicate (1 + (BS8.length bs `div` 64)) 0

buildJsonSemiIndex3
  :: ByteString
  -> (Vector Word64, Vector Word64, Vector Word64)
  -> (Vector Word64, Vector Word64, Vector Word64)
buildJsonSemiIndex3 "" bvs = bvs
buildJsonSemiIndex3 bs bvs = L.view L._1 $ L.ifoldl f (bvs, False, 0) (BS8.unpack bs)
  where
    f i ((pos, bpA, bpB), inStr, bpn) c
      | inStr    && c == '"' = ((         pos,            bpA,            bpB), False, bpn)
      | inStr    && c /= '"' = ((         pos,            bpA,            bpB), inStr, bpn)
      | c == '{' || c == '[' = ((setBit i pos, setBit bpn bpA, setBit bpn bpB), inStr, bpn + 1)
      | c == ']' || c == '}' = ((setBit i pos,            bpA,            bpB), inStr, bpn + 1)
      | c == ':' || c == ',' = ((setBit i pos,            bpA, setBit bpn bpB), inStr, bpn + 1)
      | c == '"'             = ((         pos,            bpA,            bpB), True , bpn)
      | otherwise            = ((         pos,            bpA,            bpB), inStr, bpn)

maybeClose3 :: Int -> Vector Word64 -> Vector Word64 -> Vector Word64 -> Maybe Int
maybeClose3 r positions bpA bpB
  | r == 0                                 = Nothing
  | (op - 2) .!?. bpA || (op - 2) .!?. bpB = Nothing
  | otherwise                              = go' (0::Word) bpA bpB (op - 1)
  where
    go' s ba bb cur
      | s == 0 && cur .!?. ba && cur .!?. bb = Just cur
      |           cur .!?. ba && cur .?. bb  = go' s       ba bb (cur + 1)
      |           cur .!?. ba && cur .!?. bb = go' (s - 1) ba bb (cur + 1)
      |           cur .?. ba  && cur .?. bb  = go' (s + 1) ba bb (cur + 1)
      | otherwise                            = Nothing

    op = getPos (r + (r `mod` 2)) positions

bitShowBS :: BitShow a => a -> ByteString
bitShowBS = BS8.pack . bitShow

getFinalChar :: Vector Word64 -> Vector Word64 -> Vector Word64 -> ByteString -> Int -> Maybe Char
getFinalChar positions upperBP lowerBP inp rnk =
  BS8.index inp . fromIntegral . S1.select1 positions . fromIntegral
  <$> maybeClose3 rnk positions upperBP lowerBP

buildPrintIndex :: ByteString -> IO ()
buildPrintIndex inp = do
  let
    bitv = emptyBV inp
    (positions, upperBP, lowerBP) = buildJsonSemiIndex3 inp (bitv, bitv, bitv)

  BS8.putStrLn ""
  BS8.putStrLn "BitVectors!"
  BS8.putStrLn $ "Input  " <> inp
  BS8.putStrLn $ "Length " <> bshow (BS8.length inp)
  BS8.putStrLn $ "Pos 3  " <> bitShowBS positions
  BS8.putStrLn $ "BP 3 A " <> bitShowBS upperBP
  BS8.putStrLn $ "BP 3 B " <> bitShowBS lowerBP

  print $ "Close Rank @  " <> bshow (maybeClose3 1 positions upperBP lowerBP)
  print $ "Close Char @  " <> bshow (S1.select1 positions . fromIntegral <$> maybeClose3 1 positions upperBP lowerBP)
  print $ "Close Char =  " <> bshow (getFinalChar positions upperBP lowerBP inp 1)

canHazClose :: IO ()
canHazClose = do
  let
    testPositions = [1,0,0,0,1,0,0,1,0,0,0,0,1,0,1,0,0,0,1,0,1,0,1,0,0,0,0,0,1,1,0,0,0,0,1,0,0,0,0,0,1,1]
    testBP        = "(()()()((()((()()))()())))"
    testpp        = "{\"a\": 1, \"b\": {\"l\": [1, null], \"v\": true}}"

    arr = "[[1],[5]]"
    obj = "{\"foo\":{\"a\":3},\"bar\":[5]}"

    fakeBitStr = filter (not . (`elem` ("[]," :: String))) . show
    fakeBitBS = BS8.pack . fakeBitStr

    (pos, bp) = buildJsonSemiIndex testpp

  BS8.putStrLn "*"
  BS8.putStrLn $ "Input     " <> testpp
  BS8.putStrLn $ "Length    " <> bshow (BS8.length testpp)
  BS8.putStrLn $ "Test Pos  " <> fakeBitBS testPositions
  BS8.putStrLn $ "Positions " <> fakeBitBS pos
  BS8.putStrLn $ "Test BP   " <> testBP
  BS8.putStrLn $ "BP        " <> BS8.pack bp

  buildPrintIndex testpp
  buildPrintIndex obj
  buildPrintIndex arr

