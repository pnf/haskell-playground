import  qualified Data.Map.Strict as MS
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS

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
crossOff :: (MS.Map Int IntSet) -> (MS.Map Int IntSet)
crossOff nps =
  IS.foldl' (roll np) nps' ps
  where
      (np,ps) = MS.findMin nps
      nps'    = MS.deleteMin nps
      roll :: Int -> (MS.Map Int IntSet) -> Int -> (MS.Map Int IntSet)
      roll np nps p = MS.insertWith IS.union (np+p) (IS.singleton p) nps

seive :: [Int]
seive = seive' MS.empty 2
        where
          seive' :: (MS.Map Int IntSet) -> Int -> [Int]
          seive' nps n
            | MS.null nps || n < (fst . MS.findMin $ nps)
                 = n:seive' (MS.insert (2*n) (IS.singleton n) nps) (n+1)
            | otherwise = seive' (crossOff nps) (n+1)
