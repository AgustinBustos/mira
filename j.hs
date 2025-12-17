-------------------------------------------------------------------------------------------------------------------------------------------------HS# import
:set -w

import Data.List (lines)


f<-readFile "todo.txt"
--------------------------------------------------------------------HF
Leaving GHCi.
[1]+  Done                    nohup ~/Documents/ideas/gitgraph/bashen.sh > /dev/null 2>&1
agus@agus-Inspiron-15-3567:~/Documents/ideas/jobs/mira$ cabal reple
Error: cabal: unrecognised command: reple (try --help)
Maybe you meant `repl`?
agus@agus-Inspiron-15-3567:~/Documents/ideas/jobs/mira$ cabal repl
Build profile: -w ghc-9.6.7 -O1
In order, the following will be built (use -v for more details):
 - mira-0.1.0.0 (interactive) (exe:mira) (first run)
Preprocessing executable 'mira' for mira-0.1.0.0...
GHCi, version 9.6.7: https://www.haskell.org/ghc/  :? for help
[1 of 2] Compiling Main             ( app/Main.hs, interpreted )
Ok, one module loaded.
-------------------------------------------------------------------------------------------------------------------------------------------------HS

data State = ToDo | InProgress | Done deriving Show
data Description = Description (Maybe Int) (Maybe String) (Maybe Int) (Maybe Int) | NoDescription deriving Show
data Task = Task Int State String Description deriving Show
--------------------------------------------------------------------HF
-------------------------------------------------------------------------------------------------------------------------------------------------HS# get state
ll=lines f

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
cutString x [] =[] ;
cutString x (h:t) =if h /= ':' then h:cutString x t else t

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
    repl x= x

getId row


--------------------------------------------------------------------HF
"1: map apis | 2 "
re solved =splitComma (if isInside then h/='\"' else h=='\"') t
 where solved =splitPoints (if isInside then h/='\"' else h=='\"') t
1
-------------------------------------------------------------------------------------------------------------------------------------------------HS# get task
i=cutString ':' row

task [] = [] ;
task (h:t) = if h/='|' then h:task t else [] 

task i





--------------------------------------------------------------------HF
"1 map apis "
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
<interactive>:52:21: error: [GHC-88464]
    Variable not in scope: checkChar :: Char -> [Char] -> Bool
["13","2d"]
Just 13
Just 2
Nothing
["13","2d"]
*** Exception: Prelude.!!: index too large
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/List.hs:1366:14 in base:GHC.List
  tooLarge, called at libraries/base/GHC/List.hs:1376:50 in base:GHC.List
  !!, called at <interactive>:86:20 in interactive:Ghci39
Nothing
-------------------------------------------------------------------------------------------------------------------------------------------------HS# final

ll!!6=row
row


getNode row = Task (getId row) (getState row) (task i) desc ;
  where ;
    i=cutString ':' row ;
    desc = if checkChar '|' row then d else NoDescription ;
    d=Description (conn after) (user after) (days after) (hier after) ;
    semi=(splitBar False row)!!1 ;
    after=splitComma False semi

getNode $ row
--------------------------------------------------------------------HF
"12: airbyte in prod|13,2d"
tion ;    d=Description (conn after) (user after) (days after) (hier after) ;    semi=(splitBar False row)!!1 ;    after=splitComma False semi
Task 12 ToDo "12 airbyte in prod" (Description *** Exception: <interactive>:218:1-9: Non-exhaustive patterns in function !!
-------------------------------------------------------------------------------------------------------------------------------------------------HS

--------------------------------------------------------------------HF
