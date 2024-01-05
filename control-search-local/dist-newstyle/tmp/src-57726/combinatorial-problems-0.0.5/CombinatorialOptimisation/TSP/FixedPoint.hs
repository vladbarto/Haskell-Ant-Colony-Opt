-----------------------------------------------------------------------------
-- |
-- Module      :  CombinatorialOptimisation.TSP.FixedPoint
-- Copyright   :  (c) Richard Senington 2011
-- License     :  GPL-style
-- 
-- Maintainer  :  Richard Senington <sc06r2s@leeds.ac.uk>
-- Stability   :  provisional
-- Portability :  portable
-- 
-- Simple library for fixed point arithmetic. Pure Haskell style, 
-- unlikely to be efficient. Really this has been added as a bit of 
-- a hack at the present time to remove rounding errors in the TSP 
-- implementation (which was having them from the use of Float and Double).
-- Not intended to be a full library on it's own, but I guess I see what happens.
--
-- Internally uses Int64 as the data type and this is then divided to 32 bits below 
-- the point, 31 above and the sign is still in place. 
-- Basic arithmetic becomes simple integer arithmetic (what I really really want), 
-- multiplication and division has to make use of conversion to Integer type and 
-- shifting, probably not that fast. 
----------------------------------------------------------------------------- 

module CombinatorialOptimisation.TSP.FixedPoint (FP(FP),unwrappedFP,doubleToFP,fpToDouble) where

import Data.Int
import Data.Bits
import Data.Ratio
import Foreign.C.Types


-- simple fixed point library, using 64 bit integers as the basis and 32 bits below the point (leaves 31 above, these are still signed)

fixedPoint = 32
divConstI = 2^fromIntegral fixedPoint
divConstD = 2**fromIntegral fixedPoint
divConstC :: CDouble
divConstC = fromIntegral divConstI
fpOne = fromInteger 1

newtype FP = FP Int64 deriving (Eq,Ord)

instance Show FP where
  show x@(FP a) = "FP internal:"++(show a)++"  floating:"++(show . (realToFrac :: FP->Double) $ x)

instance Num FP where
  (+) (FP a) (FP b) = FP (a+b)
  (*) (FP a) (FP b) = FP $ fromIntegral $ shiftR ((toInteger a) * (toInteger b)) fixedPoint -- bad, but will not be using it much myself
  (-) (FP a) (FP b) = FP (a-b)
  negate (FP a) = FP (-a)
  abs (FP a) = FP (abs a)
  signum (FP a) = FP (signum a)
  fromInteger i = FP (shiftL (fromInteger i) fixedPoint)

instance Fractional FP where
  (/) (FP a) (FP b) = FP $ fromInteger (div (shiftL (toInteger a) fixedPoint) (toInteger b))
  recip = (/) fpOne 
  fromRational = doubleToFP . fromRational 

instance Real FP where
  toRational (FP a) = (fromIntegral a) % divConstI

  

fpToDouble :: FP->Double
fpToDouble (FP x) = realToFrac (fromIntegral x / divConstC) 

doubleToFP :: Double->FP
doubleToFP = FP . unwrappedFP

unwrappedFP :: Double->Int64
unwrappedFP x = let (a,b) = properFraction x
                in (shiftL (fromInteger a) fixedPoint) + (floor (b * divConstD)) 
 

five,six :: FP
five = fromInteger 5
six = fromInteger 6

-- needs good test







