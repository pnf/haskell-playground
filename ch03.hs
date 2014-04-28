add a b = a + b

myCount :: [a] -> Int
myCount (x:xs) = 1 + myCount xs
myCount [] = 0
      
myCount2 :: [a] -> Int
myCount2 els = case els of
     x:xs -> 1 + myCount2 xs
     [] -> 0

sumList(x:xs) = x + sumList xs
sumList [] = 0

myMean els = (sumList els) / (fromIntegral (myCount2 els))

-- should be O(n)

myReverse :: [a] -> [a]
myReverse xs = _myReverse xs [] 
  where _myReverse :: [a] -> [a] -> [a]
        _myReverse (x:xs) acc  = _myReverse xs (x:acc)
        _myReverse [] acc = acc

toPal :: [a] -> [a]
toPal xs = xs ++ (myReverse xs)


--_isPal :: [(Eq a)] -> [(Eq a)] -> Bool

isPal xs = _isPal xs (myReverse xs)
_isPal (x:xs) (y:ys) = (x==y) && (_isPal xs ys)
_isPal [] [] = True
_isPal _ []  = False
_isPal [] _ = False


intersperse :: a -> [[a]] -> [a]
intersperse sep  [] = []
intersperse sep  [x] = x
intersperse sep  (xs:xss) = xs  ++ [sep] ++ (intersperse sep xss)

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

myDepth :: Tree a -> Int
myDepth node = _myDepth node 0
   where
    _myDepth :: Tree a -> Int -> Int
    _myDepth Empty i = i
    _myDepth (Node _ left right) i = (max (_myDepth left (i+1)) (_myDepth right (i+1)))


data Tree2 a = Node2 a (Maybe (Tree2 a)) (Maybe (Tree2 a))
             deriving (Show)


data Direction = Straight | Left | Right deriving Show
data Point = Point Float Float deriving Show

sub (Point x1 y1) (Point x2 y2) = (Point (x2 - x1) (y2 - y1))
dot (Point x1 y1) (Point x2 y2) = (x1 * x2) + (y1 * y2)
cross_k (Point x1 y1) (Point x2 y2) = (x1 * y2) - (x2 * y1)
len (Point x y) = (sqrt ((x*x) + (y*y)))
turn :: Point -> Point -> Point -> Direction
turn p1 p2 p3
    | (abs cp) < eps = Main.Straight
    | cp < 0 = Main.Right
    | cp > 0 = Main.Left
    where 
      eps = 0.01
      cp = (cross_k (sub p2 p1) (sub p3 p2))

turns :: [Point] -> [Direction]
turns ps = case ps of
     (p1:p2:p3:ps) -> (turn p1 p2 p3) : (turns (p2:p3:ps))
     ps -> []
