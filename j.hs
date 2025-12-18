-------------------------------------------------------------------------------------------------------------------------------------------------HS# import
:set -w
:r 

import Data.List (lines,find)
import Data.Time


f<-readFile "todo.txt"
ll=filter (\(h:_)->h/='#') $ lines f

ts=getNode <$> ll
--------------------------------------------------------------------HF
Ok, one module loaded.
-------------------------------------------------------------------------------------------------------------------------------------------------HS# dates
isWeekend d = wd == Saturday || wd == Sunday where wd = dayOfWeek d
addWorkDays 0 d = d ;
addWorkDays n d = if isWeekend d then addWorkDays n (addDays 1 d) else  addWorkDays (n - 1) (addDays 1 d)

start_date=fromGregorian 2025 12 17 :: Day 

:t addDays
addDays 7 start_date
addWorkDays 7 start_date

-- getNode <$> ll
--------------------------------------------------------------------HF
addDays :: Integer -> Day -> Day
2025-12-24
2025-12-26
-------------------------------------------------------------------------------------------------------------------------------------------------HS

doing_task=(filter (\x->case x of (Task  _ InProgress _ _) ->True ; _->False ) ts)!!0

getConnId (Task _ _ _ (Description con _ _ _)) = con
getConnId doing_task
--------------------------------------------------------------------HF
Just 17
-------------------------------------------------------------------------------------------------------------------------------------------------HS


nextConn node=find (\(Task i _ _ _)->Just i== next) ts where next =  getConnId node
:t doing_task
:t nextConn doing_task
:t nextConn <$> nextConn doing_task
:t nextConn
-- :i Monad
-- :t nextConn >>= nextConn doing_task
(Just doing_task >>= nextConn) >>=nextConn 
--------------------------------------------------------------------HF
doing_task :: Task
nextConn doing_task :: Maybe Task
nextConn <$> nextConn doing_task :: Maybe (Maybe Task)
nextConn :: Task -> Maybe Task
Just (Task 1 ToDo "map apis " (Description (Just 2) Nothing Nothing (Just 1)))
-------------------------------------------------------------------------------------------------------------------------------------------------HS
ordered Nothing = [];
ordered start = start:ordered next where next= start >>=nextConn 
os=ordered (Just doing_task)
--------------------------------------------------------------------HF
-------------------------------------------------------------------------------------------------------------------------------------------------HS


timedTasks=((\(Just (Task i _ _ (Description _ _ d _ )))->case d of Nothing ->(i,0);Just x -> (i,x))) <$> os
-- foldl timedTasks

acc a [] = [];
acc a ((i,days):t) = (i,a+days):acc (a+days) t

semifinal=(\(i,d)->(i,d `addWorkDays` start_date)) <$> (acc 0 timedTasks)
startandfinish last []=[];
startandfinish last ((i,d):t)=(i,last,d):startandfinish d t

startandfinish start_date semifinal

--------------------------------------------------------------------HF
[(18,2025-12-17,2025-12-17),(17,2025-12-17,2025-12-17),(1,2025-12-17,2025-12-17),(2,2025-12-17,2025-12-20),(3,2025-12-20,2025-12-24),(4,2025-12-24,2025
-12-26),(5,2025-12-26,2025-12-30),(6,2025-12-30,2026-01-01),(7,2026-01-01,2026-01-06),(8,2026-01-06,2026-01-13),(9,2026-01-13,2026-01-20),(10,2026-01-2
0,2026-01-23),(11,2026-01-23,2026-01-24),(12,2026-01-24,2026-01-28),(13,2026-01-28,2026-02-25)]
semifinal :: [(Int, Day)]
startandfinish :: t -> [(a, t)] -> [(a, t, t)]
-------------------------------------------------------------------------------------------------------------------------------------------------HS

--------------------------------------------------------------------HF
-------------------------------------------------------------------------------------------------------------------------------------------------HS

--------------------------------------------------------------------HF
