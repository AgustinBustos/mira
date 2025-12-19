module Main where

import Data.List (lines,find)
import Data.Time

main :: IO ()
main = putStrLn "Hello, Haskell!"

-------------------------------------------------------------------------------------------------------------------------------------------------HS

data State = ToDo | InProgress | Done deriving Show
data Description = Description (Maybe Int) (Maybe String) (Maybe Int) (Maybe Int) | NoDescription deriving Show
data Task = Task Int State String Description deriving Show
--------------------------------------------------------------------HF
-------------------------------------------------------------------------------------------------------------------------------------------------HS# get state
getState row = case row!!0 of '*' -> InProgress; '-' ->Done; _->ToDo
--------------------------------------------------------------------HF
-------------------------------------------------------------------------------------------------------------------------------------------------HS# get id


splitComma _ [] = [[]] 
splitComma False (',':t) =  [[]]++splitComma False t 
splitComma isInside (h:t) =  ([h] ++ head solved) : tail solved 
  where solved =splitComma (if isInside then h/='\"' else h=='\"') t

splitPoints _ [] = [[]] 
splitPoints False (':':t) =  [[]]++splitPoints False t 
splitPoints isInside (h:t) =  ([h] ++ head solved) : tail solved 
  where solved =splitPoints (if isInside then h/='\"' else h=='\"') t



getId row = num 
  where
    semiid=(splitPoints False $ (repl <$> row))!!0 
    num = read semiid :: Int 
    repl '*' = ' ' 
    repl '-' = ' ' 
    repl x= x



--------------------------------------------------------------------HF
-------------------------------------------------------------------------------------------------------------------------------------------------HS# get task

task [] = [] 
task (h:t) = if h/='|' then h:task t else [] 



--------------------------------------------------------------------HF
-------------------------------------------------------------------------------------------------------------------------------------------------HS# get tags


splitBar _ [] = [[]] 
splitBar False ('|':t) =  [[]]++splitBar False t 
splitBar isInside (h:t) =  ([h] ++ head solved) : tail solved 
  where solved =splitBar (if isInside then h/='\"' else h=='\"') t


safeSplitBar x = if checkChar '|' x then Just (splitBar False x) else Nothing


checkChar c [] = False 
checkChar c (h:t)= if c == h then True else checkChar c t


getDays row = num 
  where
    semiid=(repl <$> row) 
    num = read semiid :: Int 
    repl 'd' = ' ' 
    repl x= x

getHierarchy row = num 
  where
    semiid=(repl <$> row) 
    num = read semiid :: Int 
    repl 'h' = ' ' 
    repl x= x

days [] = Nothing 
days (h:t) = if checkChar 'd' h then Just (getDays h) else days t

hier [] = Nothing 
hier (h:t) = if checkChar 'h' h then Just (getHierarchy h) else hier t


getConnection row = num 
  where
    num = read row :: Int 


conn [] =Nothing 
conn (h:t) = if (not (checkChar 'h' h)) && (not (checkChar 'd' h)) then Just (getConnection h) else conn t



checkNumber x = or $ (`checkChar` x) <$> ['1','2','3','4','5','6','7','8','9']

user [] = Nothing 
user (h:t) = if not (checkNumber h) then Just h else user t

--------------------------------------------------------------------HF
-------------------------------------------------------------------------------------------------------------------------------------------------HS# final

getNode row = Task (getId row) (getState row) pretask2 desc 
  where 
    (_:[i]) = splitPoints False row 
    pretask = task i 
    pretask2 = case pretask of {(' ':x)-> x; x->x} 
    desc = if checkChar '|' row then d else NoDescription 
      where 
        d = Description (conn after) (user after) (days after) (hier after) 
        (_:[semi]) = (splitBar False row)  
        after=splitComma False semi 

--------------------------------------------------------------------HF
-------------------------------------------------------------------------------------------------------------------------------------------------HS# dates
isWeekend d = wd == Saturday || wd == Sunday where wd = dayOfWeek d

addWorkDays 0 d = d 
addWorkDays n d = if isWeekend d then addWorkDays n (addDays 1 d) else  addWorkDays (n - 1) (addDays 1 d)


--------------------------------------------------------------------HF
-------------------------------------------------------------------------------------------------------------------------------------------------HS

getConnId (Task _ _ _ (Description con _ _ _)) = con
getConnId (Task _ _ _ NoDescription) = Nothing

nextConn ts node=find (\(Task i _ _ _)->Just i== next) ts where next =  getConnId node

ordered ts Nothing = []
ordered ts start = start:ordered ts next where next= start >>=nextConn ts
--------------------------------------------------------------------HF
-------------------------------------------------------------------------------------------------------------------------------------------------HS

simple (Just (Task i _ d (Description _ _ (Just x) _))) =(i,d,x)
simple (Just (Task i _ d _)) =(i,d,0) 


acc a [] = []
acc a ((i,dd,days):t) = (i,dd,a+days):acc (a+days) t

startandfinish last []=[]
startandfinish last ((i,dd,d):t)=(i,dd,last,d):startandfinish d t

evilGetTimes ts start_date doing_task= startandfinish start_date semifinal 
  where 
    semifinal=(\(i,dd,d)->(i,dd,(fromIntegral d) `addDays` start_date)) <$> (acc 0 timedTasks)
    timedTasks=simple <$> os
    os=ordered ts (Just doing_task)

getTimes ts start_date doing_task= startandfinish start_date semifinal 
  where 
    semifinal=(\(i,dd,d)->(i,dd,d `addWorkDays` start_date)) <$> (acc 0 timedTasks)
    timedTasks=simple <$> os
    os=ordered ts (Just doing_task)
--------------------------------------------------------------------HF
