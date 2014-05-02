import  qualified Data.Map.Strict  as M
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Debug.Trace (trace)

isPrime :: Int -> Bool
isPrime n =
  isPrime' n 2
  where
    isPrime' :: Int -> Int -> Bool
    isPrime' n m
      | m > (floor . sqrt . fromIntegral $ n) = True
      | n `mod` m == 0 = False
      | otherwise = isPrime' n (m+1)


-- | Maintain a map of known upcoming non-primes to their prime constituents.
--   When a non-prime np is encountered, remove its entry, while adding new entries
--   np+p for each prime constituent p.
crossOff :: (M.Map Int IntSet) -> (M.Map Int IntSet)
crossOff nps =
  IS.foldr (roll np) nps' ps
  where
      (np,ps) = M.findMin nps
      nps'    = M.deleteMin nps
      roll :: Int -> Int -> (M.Map Int IntSet) ->  (M.Map Int IntSet)
      roll np p nps = M.insertWith IS.union (np+(2*p)) (IS.singleton p) nps
-- | Lazy list of prime numbers
seive ::  [Int]
seive = 2 : seive' M.empty 3
        where
          seive' :: (M.Map Int IntSet) -> Int -> [Int]
          seive' nps n
            | M.null nps || n < (fst . M.findMin $ nps)
                 = n:seive' (M.insert (3*n) (IS.singleton n) nps) (n+2)
            | otherwise = seive' (crossOff nps) (n+2)


-- r0ml's solution, which is 1.5x faster

minus (x:xs) (y:ys) = case (compare x y) of 
          LT -> x : minus  xs  (y:ys)
          EQ ->     minus  xs     ys 
          GT ->     minus (x:xs)  ys
minus  xs     _     = xs

union (x:xs) (y:ys) = case (compare x y) of 
          LT -> x : union  xs  (y:ys)
          EQ -> x : union  xs     ys 
          GT -> y : union (x:xs)  ys
union  xs     ys    = xs ++ ys

pairs ((x:xs):(ys:t)) =
  ((x : union xs ys) : pairs t )


unionAll ((x:xs):t) =
  x : union xs (unionAll (pairs t))

primes = 2 : ([3,5..] `minus` unionAll [[p*p,p*p+2*p..] | p <- primes']) 

primes' = 3 : ([5,7..] `minus` unionAll [[p*p,p*p+2*p..] | p <- primes'])   


-- Matthew Avant's non-seive solution

ma_primes :: Integral a => [a]
ma_primes = 2 : 3 : 5 : filter ma_prime [7..]

ma_prime :: Integral a => a -> Bool
ma_prime x = all (divides x) $ primesBelowSqrt x

primesBelowSqrt :: Integral a => a -> [a]
primesBelowSqrt = flip takeWhile ma_primes . (>=) . ceiling . sqrt . fromIntegral

divides :: Integral a => a -> a -> Bool
divides x y = 0 /= x `mod` y


-- http://en.literateprograms.org/Sieve_of_Eratosthenes_(Haskell)
merge :: (Ord a) => [a] -> [a] -> [a]
merge xs@(x:xt) ys@(y:yt) = 
  case compare x y of
    LT -> x : (merge xt ys)
    EQ -> x : (merge xt yt)
    GT -> y : (merge xs yt)
diff :: (Ord a) => [a] -> [a] -> [a]
diff xs@(x:xt) ys@(y:yt) = 
  case compare x y of
    LT -> x : (diff xt ys)
    EQ -> diff xt yt
    GT -> diff xs yt
primes2, nonprimes2 :: [Integer]
primes2    = [2, 3, 5] ++ (diff [7, 9 ..] nonprimes2) 
nonprimes2 = foldr1 f $ map g $ tail primes2
  where 
    f (x:xt) ys = x : (merge xt ys)
    g p         = [ n * p | n <- [p, p + 2 ..]]
