-----------------------------------------------------------------------------
-- |
-- Module      :  FileFormat.TIM
-- Copyright   :  (c) Richard Senington 2011
-- License     :  GPL-style
-- 
-- Maintainer  :  Richard Senington <sc06r2s@leeds.ac.uk>
-- Stability   :  provisional
-- Portability :  portable
-- 
-- The loading routines for the TIM file format. 
-- I am not sure what (if anything) TIM stands for.
-- The format has been used by the |International Timetabling Competition|
-- which has been run twice so far (2002,2007). Problems in this format can be found on 
-- their websites. 
----------------------------------------------------------------------------- 

module FileFormat.TIM ( RawTimeTableProblem(RawTimeTableProblem,rawNumberOfEvents,rawNumberOfRooms,
                                            rawNumberOfPeople,rawNumberOfFeatures,rawNumberOfSlotsPerDay,
                                            rawNumberOfDays,rawRoomSizes,rawPersonEventLookup,rawRoomFeatureLookup,
                                            rawEventFeatureLookup),
                        parseFile,loadTIMFileRaw,loadTIMFile,convertToConstrainedProblem,rawToCSV
)where

import CombinatorialOptimisation.TIM

import qualified Data.Array as A
import qualified Data.Map as M
import Data.List
import Control.Monad
import Text.ParserCombinators.Parsec

{-| This is intended to be an internal format only, though I will provide access and visibility to it so that 
    it can be inspected by other programs. In practice I do not expect users to operate upon the raw problem, 
    but instead upon TimeTable.-}

data RawTimeTableProblem 
  = RawTimeTableProblem { rawNumberOfEvents      :: Int
                        , rawNumberOfRooms       :: Int
                        , rawNumberOfPeople      :: Int
                        , rawNumberOfFeatures    :: Int
                        , rawNumberOfSlotsPerDay :: Int
                        , rawNumberOfDays        :: Int
                        , rawRoomSizes           :: RoomNumber->Int
                        , rawPersonEventLookup   :: PersonNumber->[EventNumber]
                        , rawRoomFeatureLookup   :: RoomNumber->[FeatureNumber]
                        , rawEventFeatureLookup  :: EventNumber->[FeatureNumber]
                        }

{-| Helper function that will not be exported. -}
readHeader :: Parser (Int,Int,Int,Int)
readHeader = do [a,b,c,d] <-replicateM 4 readNum
                return (a,b,c,d)

{-| Helper function that will not be exported. -}
readNum :: Parser Int
readNum = do as<-many digit;spaces;return $ read as

{-| Helper function that will not be exported. -}
chunkStream :: Int->[a]->[[a]]
chunkStream i [] = []
chunkStream i xs = let (as,bs) = splitAt i xs in as : chunkStream i bs

{-| Helper function not for export. -}

invertLookup :: (Ord a,Ord b,A.Ix b)=> [a]->[b]->(a->[b])->b->[a]
invertLookup baseIndices newIndices baseLookup = (A.!) (A.array (minimum newIndices,maximum newIndices) (M.toList dat))
  where
    dat'' = M.fromList $ zip newIndices (repeat [])
    dat' = foldl' f dat'' baseIndices 
    dat = M.map sort dat'
    f c k = let xs = baseLookup k 
            in foldl' (\c' k'->M.adjust (k:) k' c') c xs

{-| parseFile is a file parser for the /tim/ format. For the output format, the FullyDescriptiveTimeTableProblem data type, 
    I have included a number of slots per day and number of days. These are constants in this loading routine. 
-}

parseFile :: Parser RawTimeTableProblem   
parseFile = do (nEvents,nRooms,nFeatures,nPeople)<-readHeader
               
               roomSizes<-replicateM nRooms readNum
               personEventGrid <-replicateM (nEvents*nPeople) readNum
               roomFeatureGrid<-replicateM (nRooms*nFeatures) readNum
               eventFeatureGrid  <-replicateM (nEvents*nFeatures) readNum

               return $ RawTimeTableProblem nEvents nRooms nPeople nFeatures 9 5 
                                 (makeArrayLookup roomSizes) 
                                 (cga nEvents personEventGrid) 
                                 (cga nFeatures roomFeatureGrid) 
                                 (cga nFeatures eventFeatureGrid)
  where
    gridToLookup xs =  [b |(a,b)<-zip xs [0..],a==1]
    makeArrayLookup xs = (A.!) (A.listArray (0,length xs -1) xs)
    cga n as = makeArrayLookup . (map gridToLookup) . (chunkStream n) $ as
          
{-| Load in a TIM file, but keep the data in the original form, as a large number of grids of bits.-}

loadTIMFileRaw :: String->IO RawTimeTableProblem
loadTIMFileRaw fName = do rawContents<-readFile fName
                          let k = parse parseFile "" rawContents
                          let (Right x) = k 
                          return x

{-| Load a TIM file, and transform it into the constrained data format so that the look up tables no longer give back just ones and zeros, 
    but lists of valid options. This should be easier to work with. -}
loadTIMFile :: String->IO TimeTable
loadTIMFile s = (loadTIMFileRaw s) >>= (return . convertToConstrainedProblem)

{-| Use the raw data to constrain problem. Only rooms that can reasonably be chosen (feature and size constraints) should be available for specific events and so on. 
    In short I am doing my own constraint (hard coded urg) processing here. -}

convertToConstrainedProblem :: RawTimeTableProblem->TimeTable
convertToConstrainedProblem input 
  = TimeTable (rawNumberOfEvents input) (rawNumberOfRooms input) (rawNumberOfPeople input) (rawNumberOfDays input * rawNumberOfSlotsPerDay input)
              (rawPersonEventLookup input) eventToPerson eventToRoom (invertLookup eventList roomList eventToRoom) M.empty M.empty M.empty eventList
              (numDays-1) (numSlots-1) 0 (\x->x `div` numSlots) (\x->x `mod` numSlots) numEventsInDayEmpty 0 0 0                                     
  where
    eventToPerson = invertLookup [0 .. rawNumberOfPeople input -1] eventList (rawPersonEventLookup input)
    roomList = [0 .. rawNumberOfRooms input -1]
    eventList = [0 .. rawNumberOfEvents input -1] 
    allEventsAnywhere = M.fromList $ zip eventList (repeat roomList) -- initially any event can go anywhere
    filteredForSizes = foldl' filterOnRoomSize allEventsAnywhere eventList -- filter rooms, based upon event size/room size correlation
    filteredForFeatures = foldl' filterOnRoomFeature filteredForSizes eventList -- filter rooms, based upon event feature/room feature corelation

    filterOnRoomSize c e = let numPeople = length . eventToPerson $ e
                               previousValidRooms = c M.! e
                           in M.insert e (filter (\r->rawRoomSizes input r >= numPeople) previousValidRooms) c

    filterOnRoomFeature c e = let previousValidRooms = c M.! e
                                  eFs = rawEventFeatureLookup input e
                                  f r = let rFs = rawRoomFeatureLookup input r 
                                        in and $ map (\e->elem e rFs) eFs
                              in M.insert e (filter f previousValidRooms) c

    eventToRoom = (A.!) (A.array (0 , rawNumberOfEvents input -1) (M.toList filteredForFeatures))

    numEventsInDayEmpty = M.fromList [ ((d,p),0) | d<-[0 ..  numDays -1],p<-[0 .. rawNumberOfPeople input -1]]
    numDays = rawNumberOfDays input
    numSlots = rawNumberOfSlotsPerDay input
    

{-| This is for human readability. It will take a raw format and return a comma and new line separated format as a String. Dump the string to a file
    and it should now be easy to load into a spread sheet and inspect. I was not comfortable incoding this as a |show| function, it seems to me that there
    is far too much information here to easily display it to a user, at least in a terminal window. -}

rawToCSV :: RawTimeTableProblem->String
rawToCSV ffdttp
  = concat [header,"\n\nRoom Sizes\n",roomOutput,"\nStudent Event Table\n",studentEventTable,"\nRoom Feature Table\n",roomFeatureTable,"\nEvent Feature Table\n",eventFeatureTable]
  where
    pList = [0..rawNumberOfPeople ffdttp -1]
    rList = [0..rawNumberOfRooms ffdttp -1]
    eList = [0 .. rawNumberOfEvents ffdttp -1]
    fList = [0..rawNumberOfFeatures ffdttp -1]
    header            = concat $ (zipWith (++)) ["Number Of Events,","\nNumber Of Rooms,","\nNumber Of Features,","\nNumber Of People,","\nSlots Per Day,","\nDays,"] 
                                              (map (\f->show $ f ffdttp) [rawNumberOfEvents,rawNumberOfRooms,rawNumberOfFeatures,rawNumberOfPeople,rawNumberOfSlotsPerDay,rawNumberOfDays])  
    roomOutput = concatMap concat [["Room ",show r,",",show $ rawRoomSizes ffdttp r,"\n"] | r <- rList]
    studentEventTable = eventHeaderRow ++ (concat ['S':(show r) ++ (indexesToBools (rawNumberOfEvents ffdttp) $ rawPersonEventLookup ffdttp r)++"\n" | r<-pList])
    roomFeatureTable = featureHeaderRow ++ (concat [ 'R':(show r) ++ (indexesToBools (rawNumberOfFeatures ffdttp) $ rawRoomFeatureLookup ffdttp r)++"\n" | r<-rList])
    eventFeatureTable = featureHeaderRow ++ (concat [ 'E':(show r) ++ (indexesToBools (rawNumberOfFeatures ffdttp) $ rawEventFeatureLookup ffdttp r)++"\n" | r<-eList])
    eventHeaderRow    = (concat [ ",E"++show x   | x<-eList])++"\n"
    featureHeaderRow  = (concat [ ",F"++show x   | x<-fList])++"\n"
    indexesToBools lim xs = concat [ [',',if elem i xs then '1' else '0'] |i<-[0..lim-1]]

