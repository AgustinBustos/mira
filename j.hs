-------------------------------------------------------------------------------------------------------------------------------------------------HS# import
:set -w

import Data.List (lines)


f<-readFile "todo.txt"
ll=lines f
--------------------------------------------------------------------HF
-------------------------------------------------------------------------------------------------------------------------------------------------HS

data State = ToDo | InProgress | Done deriving Show
data Description = Description (Maybe Int) (Maybe String) (Maybe Int) (Maybe Int) | NoDescription deriving Show
data Task = Task Int State String Description deriving Show
--------------------------------------------------------------------HF
-------------------------------------------------------------------------------------------------------------------------------------------------HS# get state

--  state
length ll
row= ll!!14
getState row = case row!!0 of '*' -> InProgress; '-' ->Done; _->ToDo
getState row 
--------------------------------------------------------------------HF
16
ToDo
-------------------------------------------------------------------------------------------------------------------------------------------------HS# get id

row= ll!!1
row

--module
splitComma _ [] = [[]] ;
splitComma False (',':t) =  [[]]++splitComma False t ;
splitComma isInside (h:t) =  ([h] ++ head solved) : tail solved ;
  where solved =splitComma (if isInside then h/='\"' else h=='\"') t

splitPoints _ [] = [[]] ;
splitPoints False (':':t) =  [[]]++splitPoints False t ;
splitPoints isInside (h:t) =  ([h] ++ head solved) : tail solved ;
  where solved =splitPoints (if isInside then h/='\"' else h=='\"') t



getId row = num ;
  where;
    semiid=(splitPoints False $ (repl <$> row))!!0 ;
    num = read semiid :: Int ;
    repl '*' = ' ' ;
    repl '-' = ' ' ;
    repl x= x

getId row


--------------------------------------------------------------------HF
"1: map apis | 2 "
re solved =splitComma (if isInside then h/='\"' else h=='\"') t
 where solved =splitPoints (if isInside then h/='\"' else h=='\"') t
   repl x= x
1
-------------------------------------------------------------------------------------------------------------------------------------------------HS# get task
row
(_:[i])=splitPoints False row

task [] = [] ;
task (h:t) = if h/='|' then h:task t else [] 
i
task i



--------------------------------------------------------------------HF
"1: map apis | 2 "
" map apis | 2 "
" map apis "
-------------------------------------------------------------------------------------------------------------------------------------------------HS# get tags



row= ll!!12
row

splitBar _ [] = [[]] ;
splitBar False ('|':t) =  [[]]++splitBar False t ;
splitBar isInside (h:t) =  ([h] ++ head solved) : tail solved ;
  where solved =splitBar (if isInside then h/='\"' else h=='\"') t


safeSplitBar x = if checkChar '|' x then Just (splitBar False x) else Nothing

semi=(splitBar False row)!!1
after=splitComma False semi

checkChar c [] = False ;
checkChar c (h:t)= if c == h then True else checkChar c t






getDays row = num ;
  where;
    semiid=(repl <$> row) ;
    num = read semiid :: Int ;
    repl 'd' = ' ' ;
    repl x= x

getHierarchy row = num ;
  where;
    semiid=(repl <$> row) ;
    num = read semiid :: Int ;
    repl 'h' = ' ' ;
    repl x= x

days [] = Nothing ;
days (h:t) = if checkChar 'd' h then Just (getDays h) else days t

hier [] = Nothing ;
hier (h:t) = if checkChar 'h' h then Just (getHierarchy h) else hier t


getConnection row = num ;
  where;
    num = read row :: Int 


conn [] =Nothing ;
conn (h:t) = if (not (checkChar 'h' h)) && (not (checkChar 'd' h)) then Just (getConnection h) else conn t

after

conn after
days after
hier after

after
checkNumber x = or $ (`checkChar` x) <$> ['1','2','3','4','5','6','7','8','9']
checkNumber $ after!!2

user [] = Nothing ;
user (h:t) = if not (checkNumber h) then Just h else user t
user after

--------------------------------------------------------------------HF
"12: airbyte in prod|13,2d"
d =splitBar (if isInside then h/='\"' else h=='\"') t
["13","2d"]
Just 13
Just 2
Nothing
["13","2d"]
*** Exception: Prelude.!!: index too large
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/List.hs:1366:14 in base:GHC.List
  tooLarge, called at libraries/base/GHC/List.hs:1376:50 in base:GHC.List
  !!, called at <interactive>:131:20 in interactive:Ghci61
Nothing
-------------------------------------------------------------------------------------------------------------------------------------------------HS# final

row=ll!!3
row
:t task
--
getNode row = Task (getId row) (getState row) pretask2 desc ;
  where ;
    (_:[i]) = splitPoints False row ;
    pretask = task i ;
    pretask2 = case pretask of {(' ':x)-> x; x->x} ;
    desc = if checkChar '|' row then d else NoDescription ;
      where ;
        d = Description (conn after) (user after) (days after) (hier after) ;
        (_:[semi]) = (splitBar False row)  ;
        after=splitComma False semi 

getNode $ row
--------------------------------------------------------------------HF
"3: google | 4,2d, 1h"
task :: [Char] -> [Char]
 pretask of {(' ':x)-> x; x->x} ;    desc = if checkChar '|' row then d else NoDescription where ;        d = Description (conn after) (user after) (da
ys after) (hier after) ;        (_:[semi]) = (splitBar False row)  ;        after=splitComma False semi 
Task 3 ToDo "google " (Description (Just 4) Nothing (Just 2) (Just 1))
-------------------------------------------------------------------------------------------------------------------------------------------------HS# dates
import Data.Time
isWeekend d = wd == Saturday || wd == Sunday where wd = dayOfWeek d
addWorkDays 0 d = d ;
addWorkDays n d = if isWeekend d then addWorkDays n (addDays 1 d) else  addWorkDays (n - 1) (addDays 1 d)

start_date=fromGregorian 2025 12 17 :: Day 
start_date
addDays 7 start_date
addWorkDays 7 start_date
-- getNode <$> ll
--------------------------------------------------------------------HF
2025-12-17
2025-12-24
2025-12-26
-------------------------------------------------------------------------------------------------------------------------------------------------HS

--------------------------------------------------------------------HF
