type Cell = (Int,Int)
data MyState = Null | S Cell [Cell] String MyState deriving (Show, Eq)

up :: MyState -> MyState
up (S (0,_) _ _ _) = Null
up (S (x,y) (a:b) s p) = S (x-1,y) (a:b) "up" (S (x,y) (a:b) s p)

down :: MyState -> MyState
down (S (3,_) _ _ _) = Null
down (S (x,y) (a:b) s p) = S (x+1,y) (a:b) "down" (S (x,y) (a:b) s p)

left :: MyState -> MyState
left (S (_,0) _ _ _) = Null
left (S (x,y) (a:b) s p) = S (x,y-1) (a:b) "left" (S (x,y) (a:b) s p)

right :: MyState -> MyState
right (S (_,3) _ _ _) = Null
right (S (x,y) (a:b) s p) = S (x,y+1) (a:b) "right" (S (x,y) (a:b) s p)

contains _ [] = False
contains (a,b) ((x,y):xs) = if (a == x && b == y)
							then True
             			    else contains (a,b) xs
							
remove _ [] = []						
remove (a,b) ((x,y):xs) = if (a == x && b == y)
						  then xs
						  else (x,y):remove (a,b) xs

collect :: MyState -> MyState
collect (S (x,y) (a:b) s p) = if contains (x,y) (a:b)
							  then S (x,y) (remove (x,y) (a:b)) "collect" (S (x,y) (a:b) s p)
							  else Null
							  
removeNull [] = []					  
removeNull (x:y) = if x == Null
						then removeNull y
						else x:removeNull y
							  
nextMyStates :: MyState -> [MyState]
nextMyStates (S (x,y) (a:b) s p) = removeNull [up (S (x,y) (a:b) s p), down (S (x,y) (a:b) s p), left (S (x,y) (a:b) s p), right (S (x,y) (a:b) s p), collect (S (x,y) (a:b) s p)]

isGoal :: MyState -> Bool
isGoal (S (x,y) a s p) = if a == []
							 then True
							 else False
							 
search :: [MyState] -> MyState
search (x:y) = if isGoal x == True
			   then x
			   else search (y ++ nextMyStates x)
			   
constructSolution :: MyState -> [String]
constructSolution (S (x,y) (a:b) "" Null) = []
constructSolution (S (x,y) _ s p) = constructSolution p ++ [s]

solve :: Cell -> [Cell] -> [String]
solve x (a:b) = constructSolution (search [S x (a:b) "" Null])