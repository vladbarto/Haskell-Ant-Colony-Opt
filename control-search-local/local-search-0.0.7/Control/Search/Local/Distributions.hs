{-| Selection routines have been made generic through the 'select' and 'select'' functions, 
    which are parametrised by probability distributions. This module provides functions for 
    constructing and manipulating some example probability distributions. 
-}

module Control.Search.Local.Distributions(
  -- * Types
  Distribution,DistributionMaker,
  -- * Distributions
  normalCDF,poissonCDF,uniformCDF,firstChoice,lastChoice, 
  -- * Distribution Manipulation
  addDistributions,addDistributions',addDistributionMakers,addDistributionMakers',
  -- * Helper code
  poisson,fixEnd,normalCDF',poissonCDF',uniformCDF',
  -- * Debug code 
  cdf2pdf,distribution2SVG
) where

import Data.Number.Erf
import Data.List
import Control.Search.Local  -- not actually used, imported for documentation linking.

{-| The data type for a discrete probability distribution. -}
type Distribution = [Double]

{-| The number of elements being selected between will often vary (such as in improving neighbourhoods), 
    so it will often be necessary to construct distributions with some constant properties, but varying the 
    length. -}
type DistributionMaker = Int->Distribution

{-| Parametrised as follows;
      
      (1) mean
      
      (2) standard deviation 
     
      (3) size

    The function generates the CDF of a Normal distribution, using the mean and standard deviation, over the 
    range of discrete values [0 .. size]. The result is processed by the 'fixEnd' function, so the final 
    entry in the distribution is always 1. Over larger enough values of size this should be fine.
 -}
normalCDF :: Double->Double->DistributionMaker
normalCDF mean sd = fixEnd . normalCDF' mean sd 

{-| Parametrised as follows;
      
      (1) mean
      
      (2) standard deviation 
     
      (3) size

    The function generates the CDF of a Normal distribution, using the mean and standard deviation, over the 
    range of discrete values [0 .. size]. This function is raw, and does not use 'fixEnd'. 
-}
normalCDF' :: Double->Double->DistributionMaker
normalCDF' mean sd n = [  (1+ erf ((fromIntegral x-mean)/sd')) /2 | x<-[0..n]]
  where sd' = sd * sqrt 2

{-| Parametrised as follows;
      
      (1) mean
           
      (2) size

    This function generates the CDF of a Poisson distribution with the specified mean, over the range of 
    discrete values [0 .. size]. This function includes a call to 'fixEnd' so that the final value of the 
    distribution is always 1.
-}
poissonCDF :: Double->DistributionMaker
poissonCDF mean = fixEnd . poissonCDF' mean

{-| Parametrised as follows;
      
      (1) mean
           
      (2) size

    This function generates the CDF of a Poisson distribution with the specified mean, over the range of 
    discrete values [0 .. size]. This function gives the raw distribution.
-}
poissonCDF' :: Double->DistributionMaker
poissonCDF' mean k = map (poisson mean) [0..k]

{-| A function to generate a Uniform distribution CDF over the range [0 .. size]. For safety this includes 
    'fixEnd', though this should have no effect.-}
uniformCDF :: DistributionMaker
uniformCDF = fixEnd . uniformCDF' 

{-| The raw Uniform distribution without using 'fixEnd'.-}
uniformCDF' :: DistributionMaker
uniformCDF' sz = let sz' = fromIntegral sz in [ fromIntegral x / sz' | x<-[1..sz] ]

{-| It is more likely that when always choosing the first element of list of choices 'head' will be 
    employed, but this has been included for completeness. This is a CDF. -}
firstChoice :: DistributionMaker
firstChoice n = replicate n 1

{-| Like 'firstChoice' it is more likely that 'last' will be used for this effect. This is also a CDF. -}
lastChoice :: DistributionMaker
lastChoice n = replicate (n-1) 0 ++ [1]

{-| This is parametrised as follows;

      (1) mean 

      (2) k    

    The function generates the probability of element /k/ being chosen from a Poisson distribution with the 
    specified mean. 

    This is quite a slow function and should probably be memoized in the future.
      -}
poisson :: Double->Int->Double
poisson mean k = exp(-mean) * sum [ (mean **(fromIntegral i)) / fi | (i,fi)<-zip [0..k] factorials]  

{-| Generates the infinite sequence of factorials.-}
factorials = scanl (*) 1 [1..]

{-| The basic CDF functions (e.g. 'poissonCDF'') often do not yield /complete/ CDFs. The distributions that 
    are produced tend to 1 in the last position of the list, but do not actually get there. For these to be 
    used practically, the last value of the list should be replaced with 1. This is a bodge, but under most 
    circumstances should not adversely effect results.-}
fixEnd :: Distribution->Distribution
fixEnd = (++[1]) . init

{-| To enable the construction of more exotic distributions from existing distributions. The basic function 
    combines them with equal weight on each component. For example;

    > addDistributions [[0,0.5,1],[1,1,1]] = [0.5,0.75,1]

    Compare this with 'addDistributions''. -}
addDistributions :: [Distribution]->Distribution
addDistributions as = map (/l) . map sum . transpose $ as
  where l = fromIntegral . length $ as

{-| A more flexible variation upon 'addDistributions' which allows the programmer to specify the weights to 
    merge the distributions with. For example;

    > addDistributions [0.3,0.7] [[0,0.5,1],[1,1,1]] = [0.7,0.85,1] 
-}
addDistributions' :: [Double]->[Distribution]->Distribution
addDistributions' ps = map sum . transpose . zipWith (\p d->map (*p) d) ps 

{-| Combines 'DistributionMaker's. This combines them with equal weighting. -}
addDistributionMakers :: [DistributionMaker]->DistributionMaker
addDistributionMakers fs n = addDistributions $ map (\f->f n) fs

{-| Combines 'DistributionMaker's but weights each one.-}
addDistributionMakers' :: [Double]->[DistributionMaker]->DistributionMaker
addDistributionMakers' ps fs n = addDistributions' ps $ map (\f->f n) fs

{-| A function included for debug purposes. I expect that many users will be more comfortable using CDFs, but 
    happier thinking in terms of PDFs. Use in conjunction with 'distribution2SVG' to see what a distribution 
    looks like.-}
cdf2pdf :: Distribution->Distribution
cdf2pdf xs = head xs : zipWith (\a b->b-a) xs (tail xs)                                 

{-| A function included for debug purposes, to allow a user to visualise a distribution. 
    It generates a string, which can be stored in a file as SVG. -}
distribution2SVG :: Distribution->String
distribution2SVG d = concat [svg,
                             "<rect x=\"0\" y=\"0\" width=\"400\" height=\"300\" style=\"stroke:black;stroke-width:1;fill:white\"/>",
                             mkPath d,
                             "<text x=\"20\" y=\"350\" style=\"dominant-baseline:central\">Max Prob:",
                             show $ last d,"</text>",
                             "</svg>"
                            ]
  where
    svg = "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"400\" height=\"400\">"
    mkPath xs = let (h:hs) = [(x,y,l) | let l = fromIntegral $ length xs-1,(x,y)<-zip xs [0..] ]
                    mkM (y,x,l) = concat ["M ",show ((x/l)*400)," ",show (y*300)," "]
                    mkLs (y,x,l) = concat ["L ",show ((x/l)*400)," ",show (y*300)," "]
                in concat ["<path style=\"stroke:black;stroke-width:1;stroke-opacity:1;fill:none\" d=\"", 
                           mkM h,concatMap mkLs hs,"\"/>"]
 

