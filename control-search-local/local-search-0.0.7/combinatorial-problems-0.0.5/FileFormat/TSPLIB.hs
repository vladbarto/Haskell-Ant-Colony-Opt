-----------------------------------------------------------------------------
-- |
-- Module      :  FileFormat.TSPLIB
-- Copyright   :  (c) Richard Senington 2011
-- License     :  GPL-style
-- 
-- Maintainer  :  Richard Senington <sc06r2s@leeds.ac.uk>
-- Stability   :  provisional
-- Portability :  portable
-- 
-- Partial loading routines for the TSPLIB file format.
-- The format itself has a large number of variations, 
-- and this has only been designed to load the @tsp@ and 
-- @atsp@ variants. It has been tried on all the files
-- from the repository in these classes and it parses
-- them at least. 
--
-- Relies upon the @CombinatorialOptimisation.TSP@ library.
--
-- Currently this does not use the Haskell parsing 
-- libraries, nor ByteString, just some custom built
-- routines.
----------------------------------------------------------------------------- 

module FileFormat.TSPLIB(
  loadTSPFile
  )where

import CombinatorialOptimisation.TSP
import Data.Maybe
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.Array as A
import qualified Data.Array.Unboxed as U
import qualified Data.ByteString.Char8 as B
--import qualified Data.ByteString.Lex.Double as BL
import qualified Data.ByteString.Char8 as BS
import Data.List
import Text.Read (readMaybe)

-- going to use a fixed point internal representation, only doing addition afterall
-- not happy about how I have bolted this in.

import CombinatorialOptimisation.TSP.FixedPoint

parseDouble :: BS.ByteString -> Maybe Double
parseDouble bs = readMaybe (B.unpack bs)
  

{- |  Simple 2d Euclidian. -}

euclidianDistance :: Double->Double->Double->Double->Double 
euclidianDistance a b c d = sqrt ((a-c)*(a-c)+(b-d)*(b-d))

{- |  Always rounded up Euclidian. -}

euclidianDistanceCeil :: Double->Double->Double->Double->Double
euclidianDistanceCeil a b c d =  fromIntegral . ceiling $ euclidianDistance a b c d

{- |  For only two types of file. Basically a form of rounded Euclidian. -}

pseudoEuclideanDistance :: Double->Double->Double->Double->Double
pseudoEuclideanDistance a b c d  = let e = sqrt (((a-c)*(a-c)+(b-d)*(b-d))  /10 )
                                       f = fromIntegral (floor e)
                                   in if f<e then f+1 else f

{- |  Distance between two points on the Earth's surface. This is based upon code that 
      another developer wrote, based on the code proposed by TSPLIB itself. I am 
      not sure if this is for an earth shape, or just a sphere. I suspect earth shape. -}

geoDistance :: Double->Double->Double->Double->Double
geoDistance x1 y1 x2 y2 = encodeFloat (floor dij) 0
  where
    q1 = cos (lon1  - lon2)
    q2 = cos (lat1 - lat2)
    q3 = cos (lat1 + lat2)
    lon1 = degConvert y1
    lon2 = degConvert y2
    lat1 = degConvert x1
    lat2 = degConvert x2
    
    dij =  6378.388 * (acos( 0.5*((1.0+q1)*q2 - ((1.0-q1)*q3) )) ) + 1.0

    degConvert m = let deg = encodeFloat (floor m) 0
                       miN = m - deg
                   in 3.141592 * (deg + (5.0 * miN/3.0))/180.0

{- |  A data type for representing bits of the specification section of a file.
      Assumed to be used later as a list of Specifications. -}

data Specification = IGNORE B.ByteString 
                   | USEFUL SpecificationName B.ByteString 
                   | ENDSPEC B.ByteString 
                   | FAIL B.ByteString deriving Show
data SpecificationName = TYPE | DIMENSION | EDGEWEIGHTTYPE | EDGEWEIGHTFORMAT 
                              | DATAPARTTYPE deriving (Show, Eq)
nodeCoordConst = B.pack "NODE COORD"
edgeWeightConst = B.pack "EDGE WEIGHT"

{- |  Test for filtering specification lines, so we only get data we might want to use. Could be
      got rid of as most of the time I do dictionary lookups on the output of the specification
      reading routines. -}

isUsefulSpec (USEFUL _ _) = True
isUsefulSpec _ = False

{- |  Helper routine for @readSpecification@. Reads a single line (assumes 
      that input is already line by line). -}

readSpecificationLine :: B.ByteString->Specification
readSpecificationLine s 
  | likeString (B.pack "NAME") s = IGNORE s 
  | likeString (B.pack "TYPE") s = USEFUL TYPE (trim s)    
  | likeString (B.pack "NODE_COORD_SECTION") s = ENDSPEC nodeCoordConst
  | likeString (B.pack "EDGE_WEIGHT_SECTION") s = ENDSPEC edgeWeightConst
  | likeString (B.pack "COMMENT") s = IGNORE s
  | likeString (B.pack "DIMENSION") s = USEFUL DIMENSION (trim s)
  | likeString (B.pack "DISPLAY_DATA_TYPE") s = IGNORE s
  | likeString (B.pack "EDGE_WEIGHT_TYPE") s = USEFUL EDGEWEIGHTTYPE (trim s)
  | likeString (B.pack "EDGE_WEIGHT_FORMAT") s = USEFUL EDGEWEIGHTFORMAT (trim s)
  | otherwise = FAIL $ B.append (B.pack "unrecognised field in specification : ") s
  where
    likeString q s = B.take (B.length q) s == q
    trim s = let s' = (B.dropWhile (==' ')) . (B.drop 1) . (B.dropWhile (/=':')) $ s 
             in B.reverse . (B.dropWhile (==' ')) . B.reverse $ s'

{- |  Helper routine for @loadTSPFile@. Reads the specification section of a file. -}

readSpecification :: [B.ByteString]->([Specification],[B.ByteString])
readSpecification [] = ([FAIL $ B.pack "seem to have run out of data, without ending the specification phase"],[]) 
readSpecification (s:ss) = let p = readSpecificationLine s
                               (rs,es) = readSpecification ss
                           in case p of 
                              ENDSPEC k  -> ([USEFUL DATAPARTTYPE k],ss)
                              IGNORE _   -> (p:rs,es)
                              FAIL _     -> (p:rs,es)
                              USEFUL _ _ -> (p:rs,es)
 
 
{- |  Loads a TSPLIB style file. The first parameter is the internal 
      storage type from @CombinatorialProblems.TSP@. It allows for 
      full matrix, triangular matrix and full recalculation. If the 
      requested internal storage cannot be used with the file, this 
      will throw an error (e.g. recomputation where you are given a 
      full matrix in the file).

      The second parameter is the file path. -}

loadTSPFile :: InternalStorage->FilePath->IO TSPProblem
loadTSPFile storageType fName 
  = do rawContents<-B.readFile fName
       let (spec,remainder) = readSpecification $ B.lines rawContents
                       
       -- mapM_ print spec
       -- print ""
  
       let specList = map (\(USEFUL a b)->(a,b)) . filter isUsefulSpec $ spec
       -- print specList
       let dataPart = fromJust $ lookup DATAPARTTYPE specList
       let numNodes = fst . fromJust . B.readInt . fromJust $ lookup DIMENSION specList
       if dataPart == nodeCoordConst
        then return $ loadFromNodePositions storageType numNodes specList (myEOF remainder)
        else if dataPart == edgeWeightConst
         then return $ loadFromMatrix storageType numNodes specList (myEOF remainder)
         else error "Unsupported data section"
  where 
    myEOF = takeWhile (/= (B.pack "EOF"))

{- |  Helper routine for @loadTSPFile@. This assumes the input is just points and the 
      distances must be calculated. -}

loadFromNodePositions :: InternalStorage->Int->[(SpecificationName,B.ByteString)]->[B.ByteString]->TSPProblem
loadFromNodePositions storageType numCities spec d 
  = let d' = map ((map readF) . (drop 1) . B.words) d
        posArrA = U.listArray (0 , numCities-1) (map head d')
        posArrB = U.listArray (0 , numCities-1) (map last d')

        cities = [0 ..(numCities-1)]
        
        explicit = U.listArray (0,numCities*numCities-1)  
          [unwrappedFP $ distFunc (posArrA A.! a) (posArrB A.! a) (posArrA A.! b) (posArrB A.! b) | a<-cities,b<-cities]
        triangular = U.listArray (0,sum [0..numCities])  
          [unwrappedFP $ distFunc (posArrA A.! a) (posArrB A.! a) (posArrA A.! b) (posArrB A.! b) | a<-cities,b<-[0..a]]
      
        p = case storageType of
              ExplicitMatrix -> \x y->FP $ explicit A.! (x * numCities + y)
              TriangularMatrix -> (\x y->let x' = min x y; y' = max x y in FP $ triangular A.! (div (y'*y'+y') 2 + x'))
              Recomputation -> \a b->realToFrac $ distFunc (posArrA A.! a) (posArrB A.! a) (posArrA A.! b) (posArrB A.! b)
    in TSPProblem 0 p numCities IM.empty IM.empty
  where
    readF :: B.ByteString->Double
    --readF = fst . fromJust . BL.readDouble -- read . B.unpack 
    readF = fromJust . parseDouble
    distanceFunctionSpec = fromJust $ lookup EDGEWEIGHTTYPE spec
    distFunc = if distanceFunctionSpec == (B.pack "GEO") 
                then geoDistance 
                else if distanceFunctionSpec == (B.pack "EUC_2D")
                 then euclidianDistance
                 else if distanceFunctionSpec == (B.pack "ATT")
                  then pseudoEuclideanDistance
                  else if distanceFunctionSpec == (B.pack "CEIL_2D")
                   then euclidianDistanceCeil
                   else error $ "unsupported distance function : "++(show distanceFunctionSpec)                                  
 
{- |  Helper routine for @loadTSPFile@. This assumes the input data is a matrix of some form, 
      and loads it. -}

loadFromMatrix  :: InternalStorage->Int->[(SpecificationName,B.ByteString)]->[B.ByteString]->TSPProblem
loadFromMatrix Recomputation _ _ _ = error "Cannot load matrix and store in recomputation form"
loadFromMatrix storageType numCities spec d
  | distanceFunctionSpec /= B.pack "EXPLICIT" = error "loading from non explicit matrix? (does not make sense)"
  | storageType == TriangularMatrix = TSPProblem 0 (\x y->let x' = min x y; y' = max x y in FP $ triangularArray A.! (div (y'*y'+y') 2 + x')) numCities IM.empty IM.empty
  | storageType == ExplicitMatrix = TSPProblem 0 (\x y->FP $ explicitArray A.! (y * numCities + x)) numCities IM.empty IM.empty
  where
    readF = unwrappedFP . (read :: String->Double)  . B.unpack 
    distanceFunctionSpec = fromJust $ lookup EDGEWEIGHTTYPE spec
    inputNumberSequence = concatMap ((map readF) . B.words) d -- not entirely trusting line breaks in file format, so turning into list of numbers, and putting in coords later
    cities = [0 ..(numCities-1)]

    blankMatrix = foldl' (\m d->M.insert (d,d) 0 m) M.empty cities
    fillPair m ((a,b),c) = M.insert (b,a) c $ (M.insert (a,b) c m)
    makeFromTri = foldl' fillPair blankMatrix

    loadedMap = makeWeightsInternal (fromJust $ lookup EDGEWEIGHTFORMAT spec)
    makeWeightsInternal inp 
     | B.pack "FULL_MATRIX" == inp = foldl' (\m (d,c)->M.insert d c m) M.empty (zip [(a,b) | a<-cities,b<-cities]   inputNumberSequence)
     | B.pack "UPPER_ROW" == inp = makeFromTri $ zip [(a,b) | a<-cities,b<-[a..(numCities - 1)],a/=b] inputNumberSequence
     | B.pack "LOWER_ROW" == inp = makeFromTri $ zip [(a,b) | a<-cities,b<-[0..a],a/=b] inputNumberSequence
     | B.pack "UPPER_DIAG_ROW" == inp = makeFromTri $ zip [(a,b) | a<-cities,b<-[a..(numCities - 1)]] inputNumberSequence
     | B.pack "LOWER_DIAG_ROW" == inp = makeFromTri $ zip [(a,b) | a<-cities,b<-[0..a]] inputNumberSequence

     | B.pack "UPPER_COL" == inp = makeFromTri $ zip [(b,a) | a<-cities,b<-[a..(numCities - 1)],a/=b] inputNumberSequence
     | B.pack "LOWER_COL" == inp = makeFromTri $ zip [(b,a) | a<-cities,b<-[0..a],a/=b] inputNumberSequence
     | B.pack "UPPER_DIAG_COL" == inp = makeFromTri $ zip [(b,a) | a<-cities,b<-[a..(numCities - 1)]] inputNumberSequence
     | B.pack "LOWER_DIAG_COL" == inp = makeFromTri $ zip [(b,a) | a<-cities,b<-[0..a]] inputNumberSequence
 
    explicitArray = U.listArray (0,numCities*numCities-1) [loadedMap M.! (a,b) | a<-cities,b<-cities]
    triangularArray = U.listArray (0,sum [0..numCities])  [loadedMap M.! (a,b) | a<-cities,b<-[0..a]]

