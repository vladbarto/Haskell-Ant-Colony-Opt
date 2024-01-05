-----------------------------------------------------------------------------
-- |
-- Module      :  CombinatorialOptimisation.TIM
-- Copyright   :  (c) Richard Senington 2011
-- License     :  GPL-style
-- 
-- Maintainer  :  Richard Senington <sc06r2s@leeds.ac.uk>
-- Stability   :  provisional
-- Portability :  portable
-- 
-- A library for the representation and manipulation of Time Tabling Problems.
-- Still experimental and not particularly general. The underlying problem 
-- description is that used by the International Timetabling Competition, 
-- and the code is rather specialised towards that, with the aim of being used 
-- for meta-heuristics.
----------------------------------------------------------------------------- 

module CombinatorialOptimisation.TIM(TimeTable(TimeTable,numberOfEvents,numberOfRooms,numberOfPeople,numberOfTimeSlots,
                                               personEventLookup, eventPersonLookup,eventRoomLookup,roomEventLookup,
                                               eventLocation,locationEvent,personUsage,unscheduledEvents,overSchedule,
                                               daynumberDecode,dayslotDecode,eventsInDay,singleEventInDayCounter,lastSlotOfDayCounter,
                                               moreThanTwoEventsCounter,lastDay,lastSlotOfDay),
                                     viewConstrainedProblem,descheduleEvent,descheduleSlot,schedule,viewTimeTableDetails,
                                     ittcValidity,ittcObjectiveValue,timeTableDetailsAsCSV,timeTableForRoomAsCSV,
                                     currentlyScheduledEvents,
                                     TimeSlot,DayNumber,DaySlot,RoomNumber,EventNumber,PersonNumber,FeatureNumber,Counter
)where

import qualified Data.Array as A
import qualified Data.Map as M
import Data.List

type TimeSlot = Int
type DayNumber = Int
type DaySlot = Int
type RoomNumber = Int
type EventNumber = Int
type PersonNumber = Int
type FeatureNumber = Int
type Counter = Int

{-| Core concepts, location, timeslot, person, two events cannot happen in the same place at the same time.
    This version expects a constrained data set, so that the roomEvent lookup for example only yields events that can 
    reasonably be scheduled in that room. 

    Originally I intended the objectives (low over scheduling of people) and the soft objectives to be handled somewhere else.
    At this time, I am unsure how to abstract this, and I want a system that works now, so I will over specialise to the 
    time tabling competition specifications. Hopefully this can be rectified in a later version. -}

data TimeTable = TimeTable { numberOfEvents :: Int
                           , numberOfRooms    :: Int
                           , numberOfPeople   :: Int
                           , numberOfTimeSlots :: Int
                           , personEventLookup :: PersonNumber->[EventNumber]
                           , eventPersonLookup :: EventNumber->[PersonNumber]
                           , eventRoomLookup :: EventNumber->[RoomNumber]
                           , roomEventLookup :: RoomNumber->[EventNumber]
                           , eventLocation   :: M.Map EventNumber (TimeSlot,RoomNumber)
                           , locationEvent   :: M.Map (TimeSlot,RoomNumber) EventNumber
                           , personUsage   :: M.Map (TimeSlot,PersonNumber) Counter
                           , unscheduledEvents :: [EventNumber]
                           , lastDay :: Int
                           , lastSlotOfDay :: Int
                             -- objectives, and related code
                           , overSchedule :: Counter
                           , daynumberDecode :: TimeSlot->DayNumber
                           , dayslotDecode :: TimeSlot->DaySlot
                           , eventsInDay :: M.Map (DayNumber,PersonNumber) Counter
                           , singleEventInDayCounter :: Counter
                           , lastSlotOfDayCounter    :: Counter
                           , moreThanTwoEventsCounter :: Counter }

instance Show TimeTable where
  show t = concat ["TimeTable Problem & Solution : \n",
                   "  Validity                                   : ",if ittcValidity t then "VALID\n" else "INVALID\n",
                   "  Objective Function Value                   : ",show . ittcObjectiveValue $ t,"\n",
                   "  Over Scheduled By                          : ",show . overSchedule $ t,"\n",
                   "  Single Session In A Day Counter            : ",show . singleEventInDayCounter $ t,"\n",
                   "  Last Slot Of Day Counter                   : ",show . lastSlotOfDayCounter $ t,"\n",
                   "  More Than Two Events In Succession Counter : ",show . moreThanTwoEventsCounter $ t,"\n",
                   "  Still Unscheduled                          : ",show . length . unscheduledEvents $ t,"\n"]

{-| The objective function as specific by the 2002 competition rules. -}
ittcObjectiveValue :: TimeTable->Int
ittcObjectiveValue t = singleEventInDayCounter t +  lastSlotOfDayCounter t + moreThanTwoEventsCounter t

{-| The validity function as specific by the 2002 competition rules. Basically no clashes at this point.-}
ittcValidity :: TimeTable->Bool
ittcValidity t = (overSchedule t == 0) && (null (unscheduledEvents t))

{-| Splitting off the two parts of show, so we have a simple show for the state of the solution, 
    a more complex solution description and the constant constrained problem.
-}   
viewConstrainedProblem :: TimeTable->String
viewConstrainedProblem t = concat [header,personEventHeader,personEvent,eventPersonHeader,eventPerson,eventRoomHeader,eventRoom,roomEventHeader,roomEvent]
  where
    header = concat [concat ["Number Of ",a,",",b,"\n"] |  (a,b)<-zip ["Events","Rooms","People","Time Slots"] $ map show [numberOfEvents t,numberOfRooms t,numberOfPeople t,numberOfTimeSlots t]]
    personEventHeader = "\nPerson To Event Lookup\n"
    personEvent = concatMap concat ["\nPerson":(show p): (map (\l->","++show l)  (personEventLookup t p))    | p<-[0 .. numberOfPeople t -1]]
    eventPersonHeader = "\n\nEvent To Person Lookup\n"
    eventPerson = concatMap concat ["\nEvent":(show p): (map (\l->","++show l)  (eventPersonLookup t p))    | p<-[0 .. numberOfEvents t -1]]
    eventRoomHeader = "\n\nEvent To Room Lookup\n"
    eventRoom = concatMap concat ["\nEvent":(show p): (map (\l->","++show l)  (eventRoomLookup t p))    | p<-[0 .. numberOfEvents t -1]]
    roomEventHeader = "\n\nRoom To Event Lookup\n"      
    roomEvent = concatMap concat ["\nRoom":(show p): (map (\l->","++show l)  (roomEventLookup t p))    | p<-[0 .. numberOfRooms t -1]]


{-| The other part of the time table data type. See the current status of the solution. -}

viewTimeTableDetails :: TimeTable->String
viewTimeTableDetails t = unsched++locs
  where
    timeSlots = [0 .. numberOfTimeSlots t -1]
    roomCodes = [0 .. numberOfRooms t -1]
    persCodes = [0 .. numberOfPeople t -1]
    unsched = "Currently Unscheduled Events : "++(concat [show x++" "  |x<-unscheduledEvents t]) ++ "\n"
    locs = concat [makeTimeSlotDisplay s | s<-timeSlots,somethingAllocated s] 

    somethingAllocated s = or [M.member (s,r) (locationEvent t) | r<-roomCodes]
    personRequested s p = (M.member (s,p) (personUsage t)) && ((personUsage t) M.! (s,p) >0)
    makeTimeSlotDisplay s = concat $ ["Time Slot : ",show (daynumberDecode t s,dayslotDecode t s),"\n",
                                      makePersonUsage s]++
                                     ["  Room "++(show r)++" : "++ (show $ (locationEvent t) M.! (s,r))++"\n"  | r<-roomCodes,M.member (s,r) (locationEvent t) ]
    makePersonUsage s = "  Persons Used : "++concat [ show p++" "  |   p<-persCodes,personRequested s p]++"\n"
 
{-| A simple spread sheet display seems like a good idea. -}
timeTableDetailsAsCSV :: TimeTable->String
timeTableDetailsAsCSV t = concat [(timeTableForRoomAsCSV t r)++"\n\n" | r<-[0 .. numberOfRooms t -1]  ]       

{-| Maybe a helper, making it public anyway. -}
timeTableForRoomAsCSV :: TimeTable->RoomNumber->String
timeTableForRoomAsCSV t r = header ++ (concatMap concat [["Slot ",show s]++ [checkLocation (d * mSlots + s,r)  | d<-[0 .. mDays]]++["\n"] |s<-[0 .. mSlots]])
  where
    (days,slots) = unzip [(daynumberDecode t s,dayslotDecode t s) | s<-[0..numberOfTimeSlots t -1]]
    mDays = lastDay t 
    mSlots = lastSlotOfDay t    
    checkLocation sl | M.member sl (locationEvent t) = ","++ (show $ (locationEvent t) M.! sl)
                     | otherwise = ","
    header = ","++ concat ["Day "++(show d)++","  |   d<-[0 .. mDays]]++"\n"

{-| Fails silently and does no update the schedule if the very hard constraints fail. -}
schedule :: TimeSlot->RoomNumber->EventNumber->TimeTable->TimeTable
schedule s r e t = if validEvent && validRoom && validSlot 
                       then t{unscheduledEvents=newUnscheduled,eventLocation=el,locationEvent=le,personUsage=pu,overSchedule=newOverSchedule,eventsInDay=newEventsInDay,
                              lastSlotOfDayCounter=newLastSlot,singleEventInDayCounter=newSingleEventCounter,moreThanTwoEventsCounter = newTwoCounter}
                       else t                                   
  where
    validEvent = M.notMember e (eventLocation t)
    validRoom = elem r (eventRoomLookup t e)
    validSlot = M.notMember (s,r) (locationEvent t)
    day = daynumberDecode t s
    dayS = dayslotDecode t s

    newUnscheduled = filter (/=e) (unscheduledEvents t)
    el = M.insert e (s,r) (eventLocation t)
    le = M.insert (s,r) e (locationEvent t)
    pu = foldl' (\c k->M.alter f (s,k) c) (personUsage t) (eventPersonLookup t e)
    f Nothing = Just 1
    f p = fmap (+1) p
    newOverSchedule = (overSchedule t) +  sum [1 | p<-eventPersonLookup t e,pu M.! (s,p) >1]
    newEventsInDay = foldl' (\c p->M.alter f (day,p) c) (eventsInDay t) (eventPersonLookup t e)
    
    newLastSlot = if dayS == lastSlotOfDay t then lastSlotOfDayCounter t + (length $ eventPersonLookup t e)
                                             else lastSlotOfDayCounter t
    newSingleEventCounter = singleEventInDayCounter t + sum [1 | p<-eventPersonLookup t e,newEventsInDay M.! (day,p) == 1]
    
    beforeChain = before s dayS
    afterChain = after (lastSlotOfDay t) s dayS

    newTwoCounter = moreThanTwoEventsCounter t + (sum . (map changeInChains) $ [(findChain p beforeChain pu,findChain p afterChain pu) | p<-eventPersonLookup t e])

    
                      
changeInChains :: (Int,Int)->Int
changeInChains (0,0) = 0
changeInChains (0,1) = 0
changeInChains (1,0) = 0
changeInChains (2,0) = 1
changeInChains (0,2) = 1
changeInChains (2,1) = 2
changeInChains (1,2) = 2
changeInChains _ = 3

before :: TimeSlot->DaySlot->[TimeSlot]
before _ 0 = []
before s ds = let s' = s-1 in s':before s' (ds-1) 

after :: DaySlot->TimeSlot->DaySlot->[TimeSlot]
after l s ds | ds == l = []
             | otherwise = let s' = s+1 in s' : after l s' (ds+1)

findChain :: PersonNumber->[TimeSlot]->M.Map (TimeSlot,PersonNumber) Counter->Int
findChain p slots m = length $ takeWhile (\x->M.member (x,p) m) slots


{-| A helper method, that does not validate before descheduling. Not for export. -}
deschedule :: TimeSlot->RoomNumber->EventNumber->TimeTable->TimeTable
deschedule s r e t = t{unscheduledEvents=newUnscheduled,eventLocation=el,locationEvent=le,personUsage=pu,overSchedule=newOverSchedule,eventsInDay=newEventsInDay,
                       lastSlotOfDayCounter=newLastSlot,singleEventInDayCounter=newSingleEventCounter,moreThanTwoEventsCounter = newTwoCounter}
  where
    day = daynumberDecode t s
    dayS = dayslotDecode t s

    newUnscheduled = e:(unscheduledEvents t)
    el = M.delete e (eventLocation t)
    le = M.delete (s,r) (locationEvent t)
    pu = foldl' (\c k->M.alter f (s,k) c) (personUsage t) (eventPersonLookup t e)
    f Nothing = Nothing
    f (Just 1) = Nothing
    f (Just x) = Just (x-1)

    newLastSlot = if dayS == lastSlotOfDay t then lastSlotOfDayCounter t - (length $ eventPersonLookup t e)
                                             else lastSlotOfDayCounter t
    newSingleEventCounter = singleEventInDayCounter t - sum [1 | p<-eventPersonLookup t e,M.notMember (day,p) newEventsInDay ]
    newOverSchedule = (overSchedule t) -  sum [1 | p<-eventPersonLookup t e,M.member (s,p) pu,pu M.! (s,p) == 1]
    newEventsInDay = foldl' (\c p->M.alter f (day,p) c) (eventsInDay t) (eventPersonLookup t e)
    
    beforeChain = before s dayS
    afterChain = after (lastSlotOfDay t) s dayS

    newTwoCounter = moreThanTwoEventsCounter t - (sum . (map changeInChains) $ [(findChain p beforeChain pu,findChain p afterChain pu) | p<-eventPersonLookup t e])

{-| Fails silently if the event is not currently scheduled. -}
descheduleEvent :: EventNumber->TimeTable->TimeTable
descheduleEvent e t = if validEvent then deschedule s r e t                    
                                    else t
  where
    validEvent = M.member e (eventLocation t)
    (s,r) = (eventLocation t) M.! e

{-| Fails silently if the time slot and room number are not booked. -}
descheduleSlot :: TimeSlot->RoomNumber->TimeTable->TimeTable
descheduleSlot s r t = if validSlot then deschedule s r e t
                                    else t
  where
    validSlot = M.member (s,r) (locationEvent t)
    e = (locationEvent t) M.! (s,r)

{-| Just a combination of existing useful functions. -}
currentlyScheduledEvents :: TimeTable->[EventNumber]
currentlyScheduledEvents =  M.keys . eventLocation     

