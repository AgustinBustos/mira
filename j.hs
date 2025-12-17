-------------------------------------------------------------------------------------------------------------------------------------------------HS# import
import Data.List (lines,(!?))
f<-readFile "todo.txt"
f
--------------------------------------------------------------------HF
<interactive>:116:25: error:
    Module ‘Data.List’ does not export ‘(!?)’
"--0: jira calculation | 1\n1: map apis | 2 \n*2: shopify | 3 , 3d, 1hi \n3: google | 4,2d, 1hi \n4: bing | 5,2d, 1hi \n5: meta |6,2d, 1hi \n6: tiktok 
|7,2d, 1hi \n7: raw to silver | 8, 3d\n8: silver to sql |9,5d\n9: sql to front api  |10,5d\n10: client onboarding api|11, 3d\n11: front in azure|12,1d\
n12: airbyte in prod|13,2d\n13: security |14,20d\n14: eze help | eze\n15: keep going!!\n"
-------------------------------------------------------------------------------------------------------------------------------------------------HS
-- data TaskId = Int
data State = ToDo | InProgress | Done deriving Show
-- data Description = String
data Description = (Maybe Int) (Maybe String) (Maybe Int)

data Task = Int State String (Maybe Description)

--------------------------------------------------------------------HF
-------------------------------------------------------------------------------------------------------------------------------------------------HS# get state
ll=lines f

--  state
length ll
row= ll!!14
getState row = case row!!0 of '*' -> InProgress; '-' ->Done; _->ToDo
getState row 
--------------------------------------------------------------------HF
15
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
-------------------------------------------------------------------------------------------------------------------------------------------------HS# get thing



row= ll!!12
row

splitBar _ [] = [[]] ;
splitBar False ('|':t) =  [[]]++splitBar False t ;
splitBar isInside (h:t) =  ([h] ++ head solved) : tail solved ;
  where solved =splitBar (if isInside then h/='\"' else h=='\"') t

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
    semiid=(repl <$> row) ;
    num = read semiid :: Int


conn [] =Nothing
conn (h:t) = if (not (checkChar 'h' h)) && (not (checkChar 'd' h)) then Just (getConnection h) else conn t

conn after
days after
hier after
--------------------------------------------------------------------HF
"12: airbyte in prod|13,2d"
d =splitBar (if isInside then h/='\"' else h=='\"') t
Just 13
Just 2
Nothing
-------------------------------------------------------------------------------------------------------------------------------------------------HS

--------------------------------------------------------------------HF
-------------------------------------------------------------------------------------------------------------------------------------------------HS

--------------------------------------------------------------------HF
