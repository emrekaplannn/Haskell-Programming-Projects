module PE2 where

-- PE2: Dungeon Crawler
-- Dungeon map is :: Tree Chamber [Encounter]
-- Each encounter is either a fight or a treasure
-- Fights deal you damage (reduce HP) but enemies drop some gold (add
-- gold)
-- Tresures just give gold, or potions (which give hp)
-- Nodes hold encounters, when you visit a node you go through all of them in order
-- You start with a certain amount of HP and 0 gold.
-- You lose HP and accumulate gold as you descend the tree and go through encounters

-- Polymorphic tree structure
data Tree a b = EmptyTree | Leaf a b | Node a b [Tree a b] deriving (Show, Eq)

-- Every location in the tree is of some Chamber type.
data Chamber = Cavern |
               NarrowPassage |
               UndergroundRiver |
               SlipperyRocks deriving (Show, Eq)

-- An enemy has a name, an amount of damage that it deals
-- and an amount of gold that it drops (in that order).
data Enemy = Enemy String Integer Integer deriving (Show, Eq)

-- Gold n gives n amount of gold
-- Potion n heals n hp
data Loot = Gold Integer | Potion Integer deriving (Show, Eq)

-- An encounter is either a Fight with an Enemy, or a treasure where
-- you find Loot
data Encounter = Fight Enemy | Treasure Loot deriving (Show, Eq)

-- This is a type synonym for how we will represents our dungeons
type Dungeon = Tree Chamber [Encounter]

--Helperlar

treebulucu m
 | m==EmptyTree = "Empty Tree"
 | otherwise = treebulucu2 m

dividor1 (a,b,c) = a
dividor2 (a,b,c) = b
dividor3 (a,b,c) = c

dividorx1 (a,b) =a
dividorx2 (a,b) =b

treebulucu2 (Leaf p o) = "Leaf"
treebulucu2 (Node k l m)= "Node"

lootbulucu (Gold x) = "Gold"
lootbulucu (Potion y) = "Potion"

leafbolucu (Leaf p o) = o

nodebolucu1 (Node k l m) = l
nodebolucu2 (Node k l m) = m
nodebolucu3 (Node k l m) = k

goldamount (Treasure (Gold n)) = n
potionamount (Treasure (Potion m)) = m

fightdamage (Fight (Enemy name damage gold)) = damage
fightgold (Fight (Enemy name damage gold)) = gold

encounterbulucu (Fight (Enemy x y z)) = "Enemy"
encounterbulucu (Treasure (Gold n)) = lootbulucu (Gold n)
encounterbulucu (Treasure (Potion m)) = lootbulucu(Potion m)

traversing hp dungmap (firstchild:rest) gold
 |rest==[] = traversing2 (dividor1 (traversing2 hp dungmap firstchild gold)) (dividor3 (traversing2 hp dungmap firstchild gold)) 0 (dividor2 (traversing2 hp dungmap firstchild gold))
 |otherwise = traversing (dividor1 (traversing2 hp dungmap firstchild gold)) (dividor3 (traversing2 hp dungmap firstchild gold)) rest (dividor2 (traversing2 hp dungmap firstchild gold))

traversing2 hp dungmap childnum gold
 | (treebulucu dungmap) == "Leaf" && (leafbolucu dungmap)==[] = (hp,gold,dungmap)
 | (treebulucu dungmap) == "Leaf" = (dividorx1 (traversing3 hp (leafbolucu dungmap) gold) , dividorx2(traversing3 hp (leafbolucu dungmap) gold) , dungmap)
 | (treebulucu dungmap) == "Node" && (nodebolucu1 dungmap) == [] = (hp,gold,(dungmapfinder (nodebolucu2 dungmap) childnum))
 | (treebulucu dungmap) == "Node" = (dividorx1 (traversing3 hp (nodebolucu1 dungmap) gold) , dividorx2(traversing3 hp (nodebolucu1 dungmap) gold) ,(dungmapfinder (nodebolucu2 dungmap) childnum))

dungmapfinder (dungfirst:rest) z
 | z==0 = dungfirst
 | otherwise = dungmapfinder rest (z-1)

traversing3 hp (dungfirst:rest) gold
 |rest==[] && (encounterbulucu dungfirst) == "Enemy" = (hp-(fightdamage dungfirst) , gold+(fightgold dungfirst))
 |rest==[] && (encounterbulucu dungfirst) == "Gold" = (hp, gold+(goldamount dungfirst))
 |rest==[] && (encounterbulucu dungfirst) == "Potion" = (hp +(potionamount dungfirst), gold)
 |(encounterbulucu dungfirst) == "Enemy" = traversing3 (hp-(fightdamage dungfirst)) rest (gold+(fightgold dungfirst))
 |(encounterbulucu dungfirst) == "Gold" = traversing3 hp rest (gold+(goldamount dungfirst))
 |(encounterbulucu dungfirst) == "Potion" = traversing3 (hp +(potionamount dungfirst)) rest gold




maxfinder hp dungmap maxresult
 | (treebulucu dungmap) == "Leaf" && (leafbolucu dungmap)==[] = maxresult
 | (treebulucu dungmap) == "Leaf" && (dividorx1 (traversing3 hp (leafbolucu dungmap) 0)) <=0 = maxresult
 | (treebulucu dungmap) == "Leaf" && (dividorx1 (traversing3 hp (leafbolucu dungmap) 0)) > 0 = max2((dividorx2 (traversing3 hp (leafbolucu dungmap) maxresult)),maxresult)
 | (treebulucu dungmap) == "Node" && (nodebolucu1 dungmap) == [] =max2((maxfinder2 hp (nodebolucu2(dungmap)) maxresult), maxresult)
 | (treebulucu dungmap) == "Node" && (dividorx1 (traversing3 hp (nodebolucu1 dungmap) 0)) <=0 = maxresult
 | (treebulucu dungmap) == "Node" && (dividorx1 (traversing3 hp (nodebolucu1 dungmap) 0)) > 0 = max3((maxfinder2 (dividorx1 (traversing3 hp (nodebolucu1 dungmap) maxresult)) (nodebolucu2(dungmap)) (dividorx2 (traversing3 hp (nodebolucu1 dungmap) 0))) , (dividorx2 (traversing3 hp (nodebolucu1 dungmap) 0)),maxresult)
 
maxfinder2 hp (dungfirst:rest) maxresult
 |rest==[] = max2((maxfinder hp dungfirst maxresult ), maxresult)
 |otherwise = max3((maxfinder2 hp rest maxresult) ,maxresult , (maxfinder hp dungfirst maxresult ))

max2 (a,b)
 |a<b = b
 |otherwise = a

max3 (a,b,c)
 |a<c && b<c = c
 |a<b && c<b = b
 |otherwise = a
 

pathbulucu hp dungmap
 | (treebulucu dungmap) == "Leaf" && (leafbolucu dungmap)==[] = dungmap
 | (treebulucu dungmap) == "Leaf" && (dividorx1 (traversing3 hp (leafbolucu dungmap) 0)) <=0 = EmptyTree
 | (treebulucu dungmap) == "Leaf" && (dividorx1 (traversing3 hp (leafbolucu dungmap) 0)) > 0 = dungmap
 | (treebulucu dungmap) == "Node" && (nodebolucu1 dungmap) == [] = (Node (nodebolucu3 dungmap) (nodebolucu1 dungmap) (pathbulucu2 hp (nodebolucu2(dungmap)) ))
 | (treebulucu dungmap) == "Node" && (dividorx1 (traversing3 hp (nodebolucu1 dungmap) 0)) <=0 = EmptyTree
 | (treebulucu dungmap) == "Node" && (dividorx1 (traversing3 hp (nodebolucu1 dungmap) 0)) > 0 = (Node (nodebolucu3 dungmap) (nodebolucu1 dungmap) (pathbulucu2 (dividorx1 (traversing3 hp (nodebolucu1 dungmap) 0)) (nodebolucu2(dungmap))))
 
pathbulucu2 hp (dungfirst:rest)
 | rest==[] && (treebulucu (pathbulucu hp dungfirst)) == "Empty Tree" = []
 | rest==[] && (treebulucu (pathbulucu hp dungfirst)) == "Leaf" = [(pathbulucu hp dungfirst)]
 | rest==[] && (treebulucu (pathbulucu hp dungfirst)) == "Node" && (nodebolucu2 (pathbulucu hp dungfirst)) ==[] = [(Leaf (nodebolucu3 (pathbulucu hp dungfirst)) (nodebolucu1(pathbulucu hp dungfirst)))]
 | rest==[] && (treebulucu (pathbulucu hp dungfirst)) == "Node" && (nodebolucu2 (pathbulucu hp dungfirst)) /=[] = [(pathbulucu hp dungfirst)]
 | rest/=[] && (treebulucu (pathbulucu hp dungfirst)) == "Empty Tree" = (pathbulucu2 hp rest)
 | rest/=[] && (treebulucu (pathbulucu hp dungfirst)) == "Leaf" = [(pathbulucu hp dungfirst)] ++ (pathbulucu2 hp rest)
 | rest/=[] && (treebulucu (pathbulucu hp dungfirst)) == "Node" && (nodebolucu2 (pathbulucu hp dungfirst)) ==[] = [(Leaf (nodebolucu3 (pathbulucu hp dungfirst)) (nodebolucu1(pathbulucu hp dungfirst)))] ++ (pathbulucu2 hp rest)
 | rest/=[] && (treebulucu (pathbulucu hp dungfirst)) == "Node" && (nodebolucu2 (pathbulucu hp dungfirst)) /=[] = [(pathbulucu hp dungfirst)] ++ (pathbulucu2 hp rest)
 
 

rootfinder dungmap
 | treebulucu dungmap == "Leaf" = dungmap
 | treebulucu dungmap /= "Leaf" && (length (nodebolucu2 dungmap)) == 1 = Node (nodebolucu3 dungmap) (nodebolucu1 dungmap) [(rootfinder (helper1  (nodebolucu2 dungmap)) )]
 | max2((2 + pairdividor1 (distancefinder2 (nodebolucu2 dungmap) (0,0)) + pairdividor2 (distancefinder2 (nodebolucu2 dungmap) (0,0)) ) , distancefinder4 (nodebolucu2 dungmap) ) == (2 + pairdividor1 (distancefinder2 (nodebolucu2 dungmap) (0,0)) + pairdividor2 (distancefinder2 (nodebolucu2 dungmap) (0,0)) ) = Node (nodebolucu3 dungmap) (nodebolucu1 dungmap) (rootfinder3 (nodebolucu2 dungmap) (-1,-1))
 | max2((2 + pairdividor1 (distancefinder2 (nodebolucu2 dungmap) (0,0)) + pairdividor2 (distancefinder2 (nodebolucu2 dungmap) (0,0)) ) , distancefinder4 (nodebolucu2 dungmap) ) == distancefinder4 (nodebolucu2 dungmap) = Node (nodebolucu3 dungmap) (nodebolucu1 dungmap) [rootfinder2 (nodebolucu2 dungmap)]
 
pairdividorx1 (x1,x2) = x1
pairdividorx2 (x1,x2) = x2

isequal (a,b) (c,d)
 | a==c && b==d = 1
 | a==d && b==c = 1
 | otherwise = 0

--[Leaf UndergroundRiver [Fight (Enemy "Orc" 10 5)],
--Node Cavern [Fight (Enemy "Goblins" 5 2),Treasure (Gold 15)] [Leaf NarrowPassage [Treasure (Gold 10),Treasure (Potion 5)]]]
rootfinder3 (root1:rest) (a,b)
 | rest==[] && (isequal (distancefinder2 [root1] (a,b)) (a,b)) == 1 = []
 | rest==[]  = [rootfinder root1]
 | (isequal (distancefinder2 [root1] (a,b)) (a,b)) == 1 = rootfinder3 rest (a,b) 
 | otherwise = [(rootfinder root1)] ++ (rootfinder3 rest (distancefinder2 [root1] (a,b))) 

-- | length cevap ==2 = cevap
-- | length (root1:rest) == 2 && cevap == [] = (rootfinder3 rest (distancefinder2 [root1] (a,b) [rootfinder root1]))
-- | rest==[] && length cevap == 1 = cevap ++ [rootfinder root1]

rootfinder2 (root1:rest)
 | (distancefinder4 (root1:rest)) == (distancefinder4 [root1]) = rootfinder root1
 | otherwise = rootfinder2 rest

distancefinder dungmap
 | treebulucu dungmap=="Leaf" = 0
 | treebulucu dungmap=="Node" && (length (nodebolucu2 dungmap)) == 1 = 1 + distancefinder (helper1  (nodebolucu2 dungmap))
 | otherwise = max2((2 + pairdividor1 (distancefinder2 (nodebolucu2 dungmap) (0,0)) + pairdividor2 (distancefinder2 (nodebolucu2 dungmap) (0,0)) ) , distancefinder4 (nodebolucu2 dungmap) )

distancefinder2 (a1:rest) (x,y)
 | rest==[]  = maxc (x , y , (distancefinder3 a1))
 | otherwise = distancefinder2 rest (maxc (x , y , (distancefinder3 a1)))

distancefinder3 dungmap
 | treebulucu dungmap=="Leaf" = 0
 | treebulucu dungmap=="Node" && (length (nodebolucu2 dungmap)) == 1 = 1 + distancefinder3 (helper1  (nodebolucu2 dungmap))
 | treebulucu dungmap=="Node" && (length (nodebolucu2 dungmap)) >= 2 = 1 + distancefinder5 (nodebolucu2 dungmap)

distancefinder5 (dist1:rest)
 | rest== [] = distancefinder3 dist1
 | otherwise = max2(distancefinder3 dist1 , distancefinder5 rest)

distancefinder4 (dist1:rest)
 | rest==[] = distancefinder dist1
 | otherwise = max2(distancefinder dist1 , distancefinder4 rest )

helper1 (first:rest) = first
helper2 (first:rest) = helper1 rest
 
maxc (a,b,c)
 | a<=c && a<=b = (b,c)
 | b<=c && b<=a = (a,c)
 | otherwise = (a,b)


pairdividor1 (a,b) = a
pairdividor2 (a,b) = b 


submax (a,b) (c,d)
 | d==0 = (a,b)
 | b==0 = (c,d)
 | a<=0 = (a,b)
 | c<=0 = (c,d)
 | otherwise = submax2 (a,b) (c,d)

dividor (a,b) = div b a

submax2 (a,b) (c,d)
 | dividor (a,b) >= dividor (c,d) = (a,b)
 | otherwise = (c,d)

helpersub (a1:rest)
 | rest==[] && encounterbulucu a1 == "Enemy" = (fightdamage a1 , fightgold a1)
 | rest==[] && encounterbulucu a1 == "Gold" = (0, goldamount a1)
 | rest==[] && encounterbulucu a1 == "Potion" = (-potionamount a1 , 0)
 | encounterbulucu a1 == "Enemy" = tupleadder (fightdamage a1 , fightgold a1) (helpersub rest)
 | encounterbulucu a1 == "Gold" = tupleadder (0, goldamount a1) (helpersub rest)
 | encounterbulucu a1 == "Potion" = tupleadder (-potionamount a1 , 0) (helpersub rest)
 
tupleadder (x1,y1) (x2,y2) = (x1+x2 , y1+y2)


subtreebulucu dungmap
 | treebulucu dungmap == "Leaf" && (leafbolucu dungmap) == [] = (0,0)
 | treebulucu dungmap == "Leaf" = helpersub (leafbolucu dungmap)
 | treebulucu dungmap == "Node" && (nodebolucu1 dungmap) == [] = submax (subtreebulucu2 (nodebolucu2 dungmap)) (subtreebulucu3 (nodebolucu2 dungmap))
 | treebulucu dungmap == "Node" = submax (tupleadder (helpersub (nodebolucu1 dungmap)) (subtreebulucu2 (nodebolucu2 dungmap))) (subtreebulucu3 (nodebolucu2 dungmap))
 
subtreebulucu2 (a1:rest)
 | rest==[] = subtreebulucu4 a1
 | otherwise = tupleadder (subtreebulucu4 a1) (subtreebulucu2 rest)
 
subtreebulucu4 dungmap
 | treebulucu dungmap == "Leaf" && (leafbolucu dungmap) == [] = (0,0)
 | treebulucu dungmap=="Leaf" = helpersub (leafbolucu dungmap)
 | treebulucu dungmap == "Node" && (nodebolucu1 dungmap) == [] = (subtreebulucu5 (nodebolucu2 dungmap))
 | treebulucu dungmap == "Node" = tupleadder (helpersub (nodebolucu1 dungmap)) (subtreebulucu5 (nodebolucu2 dungmap))
 
subtreebulucu5 (a1:rest)
 | rest==[] = subtreebulucu4 a1
 | otherwise = tupleadder (subtreebulucu4 a1) (subtreebulucu5 rest)
 
subtreebulucu3 (a1:rest)
 | rest==[] = subtreebulucu a1
 | otherwise = submax (subtreebulucu a1) (subtreebulucu3 rest)

subtreefinder dungmap
 | treebulucu dungmap == "Leaf" = dungmap
 | treebulucu dungmap == "Node" && (nodebolucu1 dungmap) == [] && (submax (subtreebulucu2 (nodebolucu2 dungmap)) (subtreebulucu3 (nodebolucu2 dungmap))) == (subtreebulucu2 (nodebolucu2 dungmap)) = dungmap
 | treebulucu dungmap == "Node" && (nodebolucu1 dungmap) == [] && (submax (subtreebulucu2 (nodebolucu2 dungmap)) (subtreebulucu3 (nodebolucu2 dungmap))) == (subtreebulucu3 (nodebolucu2 dungmap)) = subtreefinder2 (nodebolucu2 dungmap) (subtreebulucu3 (nodebolucu2 dungmap))
 | treebulucu dungmap == "Node" && (submax (tupleadder (helpersub (nodebolucu1 dungmap)) (subtreebulucu2 (nodebolucu2 dungmap))) (subtreebulucu3 (nodebolucu2 dungmap))) == (tupleadder (helpersub (nodebolucu1 dungmap)) (subtreebulucu2 (nodebolucu2 dungmap))) = dungmap
 | treebulucu dungmap == "Node" && (submax (tupleadder (helpersub (nodebolucu1 dungmap)) (subtreebulucu2 (nodebolucu2 dungmap))) (subtreebulucu3 (nodebolucu2 dungmap))) == (subtreebulucu3 (nodebolucu2 dungmap)) = subtreefinder2 (nodebolucu2 dungmap) (subtreebulucu3 (nodebolucu2 dungmap))

subtreefinder2 (a1:rest) (x,y)
 | rest==[] = subtreefinder a1
 | (isequal (subtreebulucu a1) (x,y)) == 1 = subtreefinder a1
 | otherwise = subtreefinder2 rest (x,y)

 
--Ana Fonksiyonlar

-- First argument is starting HP
-- Second argument is the dungeon map
-- Third argument is the path (each integer in the list shows what child
-- you descend into)
-- Calculate how much HP you have left and how much gold you've
-- accumulated after traversing the given path
traversePath :: Integer -> Dungeon -> [Int] -> (Integer, Integer)
traversePath hp dungmap childs 
 |treebulucu dungmap == "Empty Tree" = (hp,0)
 | childs==[] = (dividor1( traversing2 hp dungmap 0 0) , dividor2(traversing2 hp dungmap 0 0) )
 |otherwise = (dividor1(traversing hp dungmap childs 0) , dividor2(traversing hp dungmap childs 0))


-- First argument is starting HP
-- Second argument is dungeon map
-- Find which path down the tree yields the most gold for you
-- You cannot turn back, i.e. you'll find a non-branching path
-- You do not need to reach the bottom of the tree
-- Return how much gold you've accumulated

findMaximumGain :: Integer -> Dungeon -> Integer
findMaximumGain hp dungmap 
 | treebulucu dungmap == "Empty Tree" = 0
 | otherwise =maxfinder hp dungmap 0


-- First argument is starting HP
-- Second argument is the dungeon map
-- Remove paths that you cannot go thorugh with your starting HP. (By
-- removing nodes from tree).
-- Some internal nodes may become leafs during this process, make the
-- necessary changes in such a case.
findViablePaths :: Integer -> Dungeon -> Dungeon
findViablePaths hp dungmap 
 | treebulucu dungmap == "Empty Tree" = EmptyTree
 |hp<0 = EmptyTree
 | hp== 0 = EmptyTree
 |(treebulucu (pathbulucu hp dungmap)) == "Empty Tree" = EmptyTree
 |(treebulucu (pathbulucu hp dungmap)) == "Leaf" = (pathbulucu hp dungmap)
 |(treebulucu (pathbulucu hp dungmap)) == "Node" && (nodebolucu2 (pathbulucu hp dungmap)) ==[] = (Leaf (nodebolucu3 (pathbulucu hp dungmap)) (nodebolucu1(pathbulucu hp dungmap)))
 |otherwise = (pathbulucu hp dungmap)


-- First argument is starting HP
-- Second Argument is dungeon map
-- Find, among the viable paths in the tree (so the nodes you cannot
-- visit is already removed) the two most distant nodes, i.e. the two
-- nodes that are furthest awat from each other.
mostDistantPair :: Integer -> Dungeon -> (Integer, Dungeon)
mostDistantPair hp dungmap
 | treebulucu (findViablePaths hp dungmap) == "Empty Tree" = (0,EmptyTree)
 | treebulucu (findViablePaths hp dungmap) == "Leaf" = (0,(findViablePaths hp dungmap))
 |otherwise= (distancefinder (findViablePaths hp dungmap), rootfinder (findViablePaths hp dungmap))


-- Find the subtree that has the highest total gold/damage ratio
-- Simply divide the total gold in the subtree by the total damage
-- in the subtree. You only take whole subtrees (i.e you can take a new
-- node as the root of your subtree, but you cannot remove nodes
-- below it). Note that the answer may be the whole tree.
mostEfficientSubtree :: Dungeon -> Dungeon
mostEfficientSubtree dungmap
 | treebulucu dungmap == "Empty Tree" = dungmap
 | treebulucu dungmap == "Leaf" = dungmap
 | otherwise = subtreefinder dungmap
 
