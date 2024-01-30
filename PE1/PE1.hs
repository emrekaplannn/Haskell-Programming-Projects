module PE1 where

-- round fonksiyonlarını ekle



import Text.Printf

-- PE1: Recipe Calculator
-- The premise of this homework if to write a recipe calculator that
-- calculates: how much a recipe costs to make, what can be made with the
-- ingredients already available, and how much extra ingredients need to
-- be bought in order to make a recipe.

-- Recipe = Recipe Name [(Ingredient, Quantity)]
data Recipe = Recipe String [(String, Double)] deriving (Show,Eq)

-- Price = Price Ingredient Quantity Price
data Price = Price String Double Double deriving Show

getunitprice (Price namee amount prc) = prc/amount
pairdividor1:: (String, Double) -> String
pairdividor1 (zzz,ttt) = zzz

pairdividor2:: (String, Double) -> Double
pairdividor2 (zz,tt) = tt


getPrice ((Price l1 l2 l3):rest) pp
 | l1==(pairdividor1 pp)= l3/l2
 | otherwise= getPrice rest pp

getInglist (Recipe stt ((kk,ll):rst)) = ((kk,ll):rst)


callrecipe ((kk,ll):rst) total v 
 |rst ==[] = total+ ll*(getPrice v (kk,ll))
 |otherwise = total+ll*(getPrice v (kk,ll)) + callrecipe rst total v

callmissing ((kk,ll):rst) stoklar result 
 |rst ==[] = result++ getmissing (kk,ll) stoklar
 |otherwise =  result++ getmissing (kk,ll) stoklar ++ callmissing rst stoklar result

getmissing (kk,ll) ((ff,ss):rst)
 |rst==[] && kk /= ff = [(kk,ll)]
 |rst==[] && kk==ff && ll<ss = []
 |rst==[] && kk==ff && ll>ss = [(kk,ll-ss)]
 |rst/=[] && kk/=ff = getmissing (kk,ll) rst
 |rst/=[] && kk==ff && ll<ss = []
 |rst/=[] && kk==ff && ll>ss = [(kk,ll-ss)]
 |otherwise = []

callstok ((kk,ll):rst) rcp result 
 |rst ==[] = result++ getremaining (kk,ll) rcp
 |otherwise =  result++ getremaining (kk,ll) rcp ++ callstok rst rcp result

getremaining (kk,ll) ((ff,ss):rst)
 |rst==[] && kk /= ff = [(kk,ll)]
 |rst==[] && kk==ff = [(kk,ll-ss)]
 |rst/=[] && kk/=ff = getremaining (kk,ll) rst
 |rst/=[] && kk==ff = [(kk,ll-ss)]



ihtiyaclist (rcp:rest) stoking fiyatlar cevap
 |rest==[] = cevap ++ fiyatekleyici (missingIngredients rcp stoking) fiyatlar
 |(missingIngredients rcp stoking) ==[] = ihtiyaclist rest (makeRecipe stoking rcp) fiyatlar cevap
 |(missingIngredients rcp stoking) /=[] = cevap ++ ihtiyaclist rest (stokguncelleyici stoking rcp) fiyatlar cevap ++ fiyatekleyici (missingIngredients rcp stoking) fiyatlar


stokguncelleyici stook rcp
 |(getInglist rcp)==[] = stook
 |stook==[] = []
 |otherwise = callstok2 stook (getInglist rcp) []
 
fiyatekleyici ((ff,ss):rst) fiyatlar 
 |rst == [] = [(ff , ss ,getIngredientCost (ff,ss) fiyatlar)]
 |otherwise = [(ff , ss ,getIngredientCost (ff,ss) fiyatlar)] ++ fiyatekleyici rst fiyatlar 
 
 
callstok2 ((kk,ll):rst) rcp result 
 |rst ==[] = result++ getremaining2 (kk,ll) rcp
 |otherwise =  result++ getremaining2 (kk,ll) rcp ++ callstok2 rst rcp result

getremaining2 (kk,ll) ((ff,ss):rst)
 |rst==[] && kk /= ff = [(kk,ll)]
 |rst==[] && kk==ff && ll<ss = [(kk,0)]
 |rst==[] && kk==ff && ll>ss = [(kk,ll-ss)]
 |rst/=[] && kk/=ff = getremaining2 (kk,ll) rst
 |rst/=[] && kk==ff && ll<ss = [(kk,0)]
 |rst/=[] && kk==ff && ll>ss = [(kk,ll-ss)]
 | otherwise = []
 
 

boss ((a,b,c):rest) cevap stringlst
 |rest==[] && (ainstringlst a stringlst)==1 = (boss2 (a,b,c) cevap)
 | rest==[] && (ainstringlst a stringlst)==0 = cevap++[(a,b,c)]
 |stringlst==[] = boss rest [(a,b,c)] [a]
 | (ainstringlst a stringlst)==1 = boss rest (boss2 (a,b,c) cevap)  stringlst
 | (ainstringlst a stringlst)==0 = boss rest (cevap++[(a,b,c)]) (stringlst++[a])


boss2 (a,b,c) ((m,n,o):rest)
 | a==m = ((m,n+b,o+c):rest)
 | a/=m = [(m,n,o)] ++ boss2 (a,b,c) rest

ainstringlst a (ss:rst)
 | rst==[] && a/=ss = 0
 | a==ss = 1
 | a/=ss = ainstringlst a rst


-- You can use this as-is
getRounded :: Double -> Double 
getRounded x = read s :: Double
               where s = printf "%.2f" x

-- Calculate how much the given amount of the given ingredient costs
getIngredientCost :: (String, Double) -> [Price] -> Double
getIngredientCost a b = getRounded ((pairdividor2 a)*(getPrice b a))


-- Calculate how much it costs to buy all the ingredients of a recipe
recipeCost :: Recipe -> [Price] -> Double
recipeCost c v 
 |(getInglist c) == [] = 0
 |otherwise = getRounded (callrecipe (getInglist c) 0.0 v)
 
-- Given a list of how much you already have of each ingredient,
-- calculate how much of which ingredients are missing for a recipe
missingIngredients :: Recipe -> [(String, Double)] -> [(String, Double)]
missingIngredients recipp stockss
 |(getInglist recipp)==[] = []
 |otherwise =callmissing (getInglist recipp) stockss []



-- Given a list of ingredients in your kitchen, calculate what you would
-- have left after making the given recipe. If there isn't enough of an
-- ingredient, the recipe cannot be made! You shouldn't change the amount
-- of ingredient in that case.
makeRecipe :: [(String, Double)] -> Recipe -> [(String, Double)]
makeRecipe stook rcp
 |(getInglist rcp)==[] = stook
 |stook==[] = []
 |(callmissing (getInglist rcp) stook []) /= [] = stook
 |(callmissing (getInglist rcp) stook []) == [] = callstok stook (getInglist rcp) []
 |otherwise = []



-- Given a list of ingredients you already have, and a list of recipes,
-- make a shopping list showing how much of each ingredient you need
-- to buy, and its cost. Each ingredient mush appear in the shopping list
-- at most once (no duplicates!).
makeShoppingList :: [(String, Double)] -> [Recipe] -> [Price] -> [(String, Double, Double)]
makeShoppingList stoking rcplistesi fiyatlar
 |rcplistesi==[] = []
 |otherwise = boss (ihtiyaclist rcplistesi stoking fiyatlar []) [] []






