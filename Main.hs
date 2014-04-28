import           Data.Char
import           Data.List
import qualified Data.Map      as Map

--import Data.Map
import           Control.Monad


main =  putStrLn "hello"
 where


  gs = ["g1","g2"]
  g2m = Map.fromList [("g1",["m1","m2"]),("g2",["m2"])]
  m2w = Map.fromList [("m1",["w1","w2"]),("m2",["w2","w3"])]

  nest xs = map (\x -> [x]) xs
  -- lookup in dictionary by first element of key list, then prepend to key list
  dget :: Ord k => Map.Map k [k] -> [k] -> [[k]]
  dget dict ks = map (:ks)  (dict Map.! (head ks))

  res =  foldl (>>=) (nest gs) [(dget g2m),(dget m2w)]

