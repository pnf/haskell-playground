{-# LANGUAGE EmptyDataDecls         #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverlappingInstances   #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE MultiWayIf   #-}
{-# LANGUAGE LambdaCase #-}

import           Data.Char
import           Data.List
import qualified Data.Map      as Map
import Control.Monad.Reader
import Control.Monad.Writer
import Data.Monoid
import Control.Monad.Instances
import Control.Applicative
import System.IO

import           Control.Monad

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show


-- head $ filter ((== 40) . fst)  $ zip (map ds [1..]) [1..]

ft :: Int -> Maybe Int
ft n = find (\x -> digitSum x == n) [1..]


phoneBook :: Map.Map String String
phoneBook = Map.fromList $
    [("betty", "555-2938")
    ,("bonnie", "452-2928")
    ,("patsy", "493-2928")
    ,("lucille", "205-2928")
    ,("wendy", "939-8282")
    ,("penny", "853-2492")
    ]

data Point = Point Float Float deriving (Show)

data Blort = Blort String Point deriving (Show)


foo :: Blort -> Float
foo (Blort s (Point x y)) = (x * y)  + (fromIntegral . length $ s)

data Bingo = Bingo { howdy :: String, doody :: Float } deriving Show

infixr 5 :-:
data LL a = EmptyList | a :-: (LL a) deriving (Show,Read,Eq,Ord)

data Tree a = EmptyTree | Node a (Tree a) (Tree a)
    deriving (Show)

singletonTree :: a  -> Tree a
singletonTree x = Node x EmptyTree EmptyTree

insertTree :: Ord a => Tree a -> a -> Tree a
insertTree EmptyTree x = Node x EmptyTree EmptyTree
insertTree tree@(Node x l r) y
  | x==y = tree
  | x>y  = Node x (insertTree l y) r
  | y>x  = Node x l (insertTree r y)

elemTree :: Ord a => Tree a -> a -> Bool
elemTree EmptyTree _ = False
elemTree (Node x l r) y
    | x==y = True
    | x>y  = elemTree l y
    | x<y  = elemTree r y



class YesNo a where
    yesno :: a -> Bool

instance YesNo [a] where
    yesno [] = False
    yesno _  = True

instance YesNo Bool where
    yesno x = x

instance (YesNo a) => YesNo (Maybe a) where
    yesno Nothing = False
    yesno (Just x) = yesno x

{--
--}

{--
instance YesNo (Maybe a) where
    yesno Nothing = False
    yesno _ = True
--}

{--
instance YesNo (Maybe a) where
    yesno Nothing = False
    yesno _ = True

--}

{--

class OuiNon a where
    ouinon :: a -> Bool

class OuiNon' flag a where
    ouinon' :: flag -> a -> Bool

instance (OuiNonPred a flag, OuiNon' flag a) => OuiNon a where
    ouinon = ouinon' (undefined::flag)

class OuiNonPred a flag | a->flag where {}

instance (flag ~ HFalse) => OuiNonPred a flag
instance OuiNonPred Bool HTrue
instance Num a => OuiNonPred a  HTrue
instance OuiNonPred a flag => OuiNonPred (Maybe a) flag

data HTrue
data HFalse

instance OuiNon a => OuiNon' HTrue (Maybe a) where
    ouinon' _ Nothing  = False
    ouinon' _ (Just x) = ouinon x

instance OuiNon' HTrue Bool where
    ouinon' _ x = x

instance (Ord a,Num a) => OuiNon' HTrue a where
    ouinon' _ x = x > 0

instance OuiNon' HFalse a where
    ouinon' _ x = False
--}


gs = ["g1","g2"]
g2m = Map.fromList [("g1",["m1","m2"]),("g2",["m2"])]
m2w = Map.fromList [("m1",["w1","w2"]),("m2",["w2","w3"])]


nest xs = map (\x -> [x]) xs

-- lookup in dictionary by first element of key list, then prepend to key list
dget :: Ord k => Map.Map k [k] -> [k] -> [[k]]
dget dict ks = map (:ks)  (dict Map.! (head ks))

res =  foldl (>>=) (nest gs) [(dget g2m),(dget m2w)]

ink x = Just (x+1)
dub x = Just (x*2)

inkMe :: Num a => Maybe a -> Maybe a
inkMe me = me >>= ink >>= dub


inkMe2 :: Num a => Maybe a -> Maybe a
inkMe2 me = do
  x <- me
  y <- ink x
  z <- dub y
  return z

data Garbage = Garbage {boffo :: String, wiz :: Int} deriving Show

compy :: Reader Garbage (Maybe String)
compy = undefined

logme :: Int -> Writer [String] Int
logme x = writer (x,["Logging " ++ (show x)])



hello :: Reader String String
hello = do { name <- ask;  return ("hello, " ++ name ++ "!")}
bye :: Reader String String
bye = do { name <- ask; return ("bye, " ++ name ++ "!")}
 
convo :: Reader String String
convo = do
    c1 <- hello
    c2 <- bye
    return $ c1 ++ c2
convo2 :: Reader String String
convo2 = hello >>= \c1 -> bye >>= \c2 -> return (c1 ++ c2)

-- runReader convo2 $ "hello"

-- Pattern guards 
combine d x y
 | Just a <- Map.lookup x d , Just b <- Map.lookup y d = Just (a ++ b)
 | otherwise = Nothing

-- multiwayif
mwi x = if
     | x > 100   -> 3  
     | x > 10    -> 2  
     | x > 1     -> 1  
     | otherwise -> 0

-- LambdaCase
lc = \case {Just y -> "just"; Nothing -> "nada"}

capslock = do
  contents <- readFile "boffo"
  putStr (map toUpper contents)

-- Equivalently
--  (readFile "boffo") >>= putStr . map toUpper
