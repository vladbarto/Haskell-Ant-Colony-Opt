-----------------------------------------------------------------------------
-- |
-- Module      :  FileFormat.SATLIB
-- Copyright   :  (c) Richard Senington 2011
-- License     :  GPL-style
-- 
-- Maintainer  :  Richard Senington <sc06r2s@leeds.ac.uk>
-- Stability   :  provisional
-- Portability :  portable
-- 
-- The loading routines for the Conjunctive Normal Form (cnf) styled files
-- that can be found on the SATLIB website. Relies upon the
-- @CombinatorialOptimisation.SAT@ library for the data structures.
----------------------------------------------------------------------------- 

module FileFormat.SATLIB(loadCNFFile,saveAsCNF)where

import CombinatorialOptimisation.SAT

import Data.List
-- import qualified Data.Map as M
import qualified Data.Array as A
import qualified Data.IntMap as IM

{- | Loading routine that takes the file path and returns a SATProblem. All variables will be set to false in the initial 
setup, and the truth values of all clauses set appropriately. -}

loadCNFFile :: FilePath->IO(SATProblem)
loadCNFFile fName 
  = do rawContents<-readFile fName
       let ls = (filter (\x->head x /= 'c')) $ lines rawContents
       let problemLine = words $ head $ filter (\x->head x == 'p') ls
       let (varCount,clauseCount) = if  problemLine !! 1 /= "cnf" then error "This is not a CNF file"
                                                                  else (read $ problemLine !! 2,read $ problemLine !! 3) :: (Int,Int)
       let clauseLines =  (map processClause) . (mySplit 0)   .  (map read) .  (concatMap words) . tail $ ls 
       let clauseMap = foldl f (IM.fromList (zip [0 .. varCount -1] $ repeat ([],[]))) (zip [0..] clauseLines)
       let varLook = ((A.listArray (0,varCount -1) (IM.elems clauseMap)) A.!)
       let claLook = ((A.listArray (0,clauseCount -1) clauseLines) A.!)
       return $ satproblem clauseCount varCount varLook claLook (IM.fromList $ zip [0 .. varCount -1] $ repeat False)
  where
    mySplit target xs = mySplit' [] xs
      where 
        mySplit' [] [] =[]
        mySplit' ns [] = [reverse ns]
        mySplit' ns (x:xs) | x == target = (reverse ns) : mySplit' [] xs
                           | otherwise = mySplit' (x:ns) xs
    processClause cs = let (as,bs) = partition (>0) cs in (map ((+) (-1)) as,map ((+) (-1)) $ map abs bs)
    f m (clauseIndex,(ords,negs)) = let addNeg m' x = IM.adjust (\(as,bs)->(as,clauseIndex:bs)) x m'
                                        addOrd m' x = IM.adjust (\(as,bs)->(clauseIndex:as,bs)) x m'
                                    in foldl addNeg (foldl addOrd  m  ords) negs 

{- | Save routine for SATProblem, outputs back into SATLIB cnf format. The code @(loadCNFFile f) >>= (saveAsCNF f)@ should 
have no effect upon the file. All information such as variable settings and the truth values of clauses is lost.
To save extra information use standard prelude write file function with show. I will try to improve on that 
at some point. -}

saveAsCNF :: FilePath->SATProblem->IO ()
saveAsCNF fName s = writeFile fName $ fixedHeader++problemHeader++concatMap prepareClause [0.. (numClauses s)-1]
  where
    fixedHeader = concat ["c\n","c SAT instance in DIMACS CNF input format.\n","c\n"]
    problemHeader = concat ["p cnf ",show . numVariables $ s," ",show . numClauses $ s,"\n"]
    prepareClause c = let (as,bs) = clauseLookUp s c
                          (as',bs') = (map (\a->(a,a+1)) as,map (\a->(a,-(a+1))) bs)
                          cs = map snd (sortBy (\a b->compare (fst a) (fst b)) $ as' ++ bs')
                      in (init . init . concat $ [show k++"  " | k<-cs ++ [0]]) ++ "\n"
                         


