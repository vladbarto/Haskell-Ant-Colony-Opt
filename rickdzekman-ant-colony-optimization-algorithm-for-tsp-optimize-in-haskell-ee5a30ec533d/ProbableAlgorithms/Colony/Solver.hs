{-# LANGUAGE RecordWildCards #-}

module ProbableAlgorithms.Colony.Solver (runSolver) where

import Problems.NPHARD.TSP
import ProbableAlgorithms.Colony.Ants

import Prelude hiding (mapM_, foldr, concat)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Foldable (foldrM, mapM_, foldr, concat)
import Data.Maybe
import Control.Lens

import Control.Applicative
import Control.Monad hiding (mapM_)
import Control.Monad.Random
import Control.Monad.State.Strict hiding (mapM_)
import Control.Monad.Reader hiding (mapM_)
import Control.Monad.Writer.Strict hiding (mapM_)
import Control.Monad.Trans.Maybe

type SolutionState = AntColony

type SolverT a = MaybeT
                    (WriterT Leaderboard
                        (ReaderT (Country, AntConfig)
                            (StateT SolutionState
                                (Rand StdGen)))) a

embed :: Maybe a -> SolverT a
embed = MaybeT . return

randomCity :: SolverT City
randomCity = do
    co <- asks fst
    let cits = getCities co
    x <- getRandomR (0, M.size cits - 1)
    embed $ M.lookup x cits

scentMapLookup :: Edge -> SolverT ScentPoints
scentMapLookup e = do
    m <- use colmap
    embed . M.lookup e $ getMap m

newPath :: City -> Path
newPath = Path [] . (:[])

newCityAnt :: Ant -> City -> SolverT CityAnt
newCityAnt a c = do
    cfg <- asks snd
    return $ CityAnt $ MainAnt a (newPath c) (scentScore cfg)

reviveDead :: SolverT ()
reviveDead = do
        oldcants <- use cants
        olddants <- use dants
        (nca,nda) <- foldrM foldfunc (oldcants,S.empty) (S.toAscList olddants)
        cants .= nca
        dants .= nda
        return ()
    where
        foldfunc :: DeadAnt -> (S.Set CityAnt,S.Set DeadAnt) -> SolverT (S.Set CityAnt,S.Set DeadAnt)
        foldfunc d (sca,sda)
            | spawnTime d == 0 = do
                nc <- randomCity
                sca' <- S.insert <$> newCityAnt (deadAnt d) nc <*> return sca
                return (sca',sda)
            | otherwise = let sda' = DeadAnt (deadAnt d) (spawnTime d - 1) `S.insert` sda
                          in  return (sca,sda')

exploreEdges :: SolverT ()
exploreEdges = do
        neweas <- use cants 
                    >>= 
                        \x -> use eants >>= 
                            \y -> foldrM (\a acc -> flip S.insert acc <$> makeEdgeAnt a) y x
        eants .= neweas
        cants .= S.empty
        return ()
    where
        replicaFromScent :: Edge -> SolverT [Edge]
        replicaFromScent er = flip replicate er <$> scentMapLookup er
        currentCity :: CityAnt -> City
        currentCity = head . cityPath . getPath . cityAnt
        makeEdgeAnt :: CityAnt -> SolverT EdgeAnt
        makeEdgeAnt ca@CityAnt{..} = do
            (ne,ixs) <- makeChoice ca
            co <- asks fst
            let cits = getCities co
                cit = fromJust $ M.lookup ixs cits
            cfg <- asks snd
            let oldPath = getPath cityAnt
                newPathExplore = Path (ne : edgePath oldPath) (cit : cityPath oldPath)
                newScent = getScent cityAnt + distanceBonus cfg
                newMainAnt = MainAnt (getAnt cityAnt) newPathExplore newScent
            return $ EdgeAnt newMainAnt ne cit (edgeWeight ne)
        makeChoice :: CityAnt -> SolverT (Edge,Id)
        makeChoice ca = (cityChoices ca <$> edgeChoices ca) >>= uniform
        cityChoices :: CityAnt -> [Edge] -> [(Edge,Id)]
        cityChoices CityAnt{..} = map mapFunc
            where
                mapFunc :: Edge -> (Edge,Id)
                mapFunc Edge{..} = if cityLeft == getId (currentCity CityAnt{..}) then (Edge{..},cityRight) else (Edge{..},cityLeft)
        edgeChoices :: CityAnt -> SolverT [Edge]
        edgeChoices CityAnt{..} = concat <$> foldrM foldFunc [] (edges (currentCity CityAnt{..}))
            where
                foldFunc :: Edge -> [[Edge]] -> SolverT [[Edge]]
                foldFunc x accum
                    | not (null . edgePath . getPath $ cityAnt) && (x == head (edgePath . getPath $ cityAnt)) = return accum
                    | otherwise = (: accum) <$> replicaFromScent x

advanceEdges :: SolverT ()
advanceEdges = use eants >>= mapM_ foldfunc
    where
        foldfunc :: EdgeAnt -> SolverT ()
        foldfunc EdgeAnt{..}
            | waitTime > 1 = do
                let ea = EdgeAnt { waitTime = waitTime - 1, ..}
                eants %= S.insert ea
            | otherwise = checkForProgress EdgeAnt{..}
        checkForProgress :: EdgeAnt -> SolverT ()
        checkForProgress ea@EdgeAnt{..} = do
            co <- asks fst
            extension . extendPath (getPath edgeAnt) $ co
            where
                extension :: ExtendPath -> SolverT ()
                extension BrokePath{} = error "A broken path? Well, this should never happen"
                extension IncPath{} = do
                    cants %= S.insert (CityAnt edgeAnt)
                    eants %= S.delete ea
                    return ()
                extension (LoopPath Path{..}) = do
                    tell . SLeaderboard . flip (SubLeaderboard (length edgePath)) Path{..} $ foldr ((+) . edgeWeight) 0 edgePath
                    eants %= S.delete ea
                    dsp <- asks (deathSpawnTime . snd)
                    dants %= S.insert (DeadAnt (getAnt edgeAnt) dsp)
                    return ()
                extension (Cycle Path{..}) = do
                    let hcyc = hcycleFromPath Path{..}
                        cycscore = weightscore hcyc
                    tell . MLeaderboard $ MainLeaderboard (M.singleton cycscore [hcyc]) (S.singleton hcyc)
                    eants %= S.delete ea
                    dants %= S.insert (DeadAnt (getAnt edgeAnt) 0)
                    return ()

cleanScents :: SolverT ()
cleanScents = do
    df <- asks (decayFunction . snd)
    bs <- asks (edgeBaseScent . snd)
    let perEdge x = let x' = df x in if x' <= bs then bs else x'
    sm <- use colmap
    colmap .= ScentMap (M.map perEdge $ getMap sm)
    return ()

solverRounds :: Int -> SolverT ()
solverRounds = flip replicateM_ (reviveDead >> exploreEdges >> advanceEdges >> cleanScents)

finaliseSolver :: StdGen -> Country -> AntConfig -> SolverT () -> (String,Leaderboard)
finaliseSolver gen co ac st = (openingStr ++ "\n" ++ show col ++ "\n-----\n\n" ++ show l,l)
    where
        ((mr,l),col) = flip evalRand gen . flip runStateT (blankColony ac co) . flip runReaderT (co,ac) . runWriterT $ runMaybeT st
        openingStr = if isNothing mr
                            then "There was an error running the solver: Unexpected Maybe (likely from Map Lookup). State prior to error:\n"
                            else "Completed Ant Colony Algorithm:\n"

runSolver :: StdGen -> Country -> AntConfig -> (String,Leaderboard)
runSolver gen co ac = finaliseSolver gen co ac . solverRounds $ noOfRounds ac

