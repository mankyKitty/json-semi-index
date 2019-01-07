{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
module Lib (canHazClose, buildPrintIndex) where

import           Control.Lens                              ((^.))

import           Data.ByteString                           (ByteString)
import qualified Data.ByteString.Char8                     as BS8
import           HaskellWorks.Data.Bits.BitShow
import qualified HaskellWorks.Data.RankSelect.Base.Select1 as S1

import qualified Lib.Simple                                as SI1
import qualified Lib.WordVector                            as WV

bshow :: Show a => a -> BS8.ByteString
bshow = BS8.pack . show

bitShowBS :: BitShow a => a -> ByteString
bitShowBS = BS8.pack . bitShow

buildPrintIndex :: ByteString -> IO ()
buildPrintIndex inp = do
  let
    bpSI      = WV.buildJsonSemiIndex inp

    positions = bpSI ^. WV.bpSIPositions
    upperBP   = bpSI ^. WV.bpSIUpperBP
    lowerBP   = bpSI ^. WV.bpSILowerBP

  BS8.putStrLn ""
  BS8.putStrLn "BitVectors!"
  BS8.putStrLn $ "Input    " <> inp
  BS8.putStrLn $ "Length   " <> bshow (BS8.length inp)
  BS8.putStrLn $ "Pos      " <> bitShowBS positions
  BS8.putStrLn $ "BP Upper " <> bitShowBS upperBP
  BS8.putStrLn $ "BP Lower " <> bitShowBS lowerBP

  print $ "Close Rank @  " <> bshow (WV.findClose 1 bpSI)
  print $ "Close Char @  " <> bshow (S1.select1 positions . fromIntegral <$> WV.findClose 1 bpSI)
  print $ "Close Char =  " <> bshow (WV.getFinalChar bpSI inp 1)

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

    (pos, bp) = SI1.buildJsonSemiIndex testpp

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

