-------------------------------------------------------------------------------------------------------------------------------------------------HS# import
:set -w
:r 

import Data.List (lines,find)
import Data.Time


f<-readFile "todo.txt"


ll=filter (\(h:_)->h/='#') $ filter (\x->x/="") $ lines f
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

doing_tasks=(filter (\x->case x of (Task  _ InProgress _ _) ->True ; _->False ) ts)
doing_tasks
doing_task=doing_tasks!!1

getConnId (Task _ _ _ (Description con _ _ _)) = con;
getConnId (Task _ _ _ NoDescription) = Nothing
getConnId doing_task

--------------------------------------------------------------------HF
[Task 2 InProgress "shopify " (Description (Just 32) Nothing (Just 3) (Just 33)),Task 40 InProgress "fill gantt " (Description (Just 36) Nothing Nothin
g Nothing)]
Just 36
-------------------------------------------------------------------------------------------------------------------------------------------------HS

nextConn node=find (\(Task i _ _ _)->Just i== next) ts where next =  getConnId node

ordered Nothing = [];
ordered start = start:ordered next where next= start >>=nextConn 
os=ordered (Just doing_task)

os
--------------------------------------------------------------------HF
[Just (Task 40 InProgress "fill gantt " (Description (Just 36) Nothing Nothing Nothing)),Just (Task 36 ToDo "ask cursor for data " (Description (Just 3
8) Nothing Nothing Nothing)),Just (Task 38 ToDo "check if airbyte does some simplification" NoDescription)]
-------------------------------------------------------------------------------------------------------------------------------------------------HS

simple (Just (Task i _ d (Description _ _ (Just x) _))) =(i,d,x);
simple (Just (Task i _ d _)) =(i,d,0) 

-- timedTasks=((\(Just (Task i _ _ (Description _ _ d _ )))->case d of Nothing ->(i,0);Just x -> (i,x))) <$> os
timedTasks=simple <$> os
-- foldl timedTasks

acc a [] = [];
acc a ((i,dd,days):t) = (i,dd,a+days):acc (a+days) t

semifinal=(\(i,dd,d)->(i,dd,d `addWorkDays` start_date)) <$> (acc 0 timedTasks)
startandfinish last []=[];
startandfinish last ((i,dd,d):t)=(i,dd,last,d):startandfinish d t

startandfinish start_date semifinal

--------------------------------------------------------------------HF
[(40,"fill gantt ",2025-12-17,2025-12-17),(36,"ask cursor for data ",2025-12-17,2025-12-17),(38,"check if airbyte does some simplification",2025-12-17,
2025-12-17)]
-------------------------------------------------------------------------------------------------------------------------------------------------HS

--------------------------------------------------------------------HF
-------------------------------------------------------------------------------------------------------------------------------------------------HS

--------------------------------------------------------------------HF
