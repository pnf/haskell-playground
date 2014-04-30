{-# LANGUAGE OverlappingInstances, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances, TypeFamilies, ScopedTypeVariables, IncoherentInstances #-}

import Data.Maybe (isJust)

class YesNo a where
  yesno :: a -> Bool

class YesNo' flag a where
  yesno' :: flag -> a -> Bool

-- functional dependency: says flag is uniquely determined by a
class MaybePred a flag | a->flag where {}

instance (flag ~ HFalse) => MaybePred a flag
instance MaybePred (Maybe a) HTrue

data HTrue
data HFalse

instance YesNo' HTrue (Maybe a) where
  yesno' _ (Just x) = True
  yesno' _ Nothing = False

instance (YesNo a) => YesNo' HFalse (Maybe a) where
  yesno' _ (Just x) = yesno x
  yesno' _ Nothing = False

instance YesNo' b a where
  yesno' _ _ = True


-- a YesNo for which there is a (MaybePred a flag), where flag is uniquely determined
-- by a, and a (YesNo' flag a).
instance (MaybePred a flag, YesNo' flag a) => YesNo a where 
  yesno = yesno' (undefined :: flag)

instance YesNo String where
  yesno ('y':_) = True
  yesno ('Y':_) = True
  yesno _ = False

instance YesNo [a] where
  yesno [] = False
  yesno _ = True

main = do
  print (yesno (Just 3))
  print (yesno (Just (1,"abc")))
  print (yesno (1,"abc"))
  print (yesno (Just "string"))
  print (yesno (Just "yes"))
  print (yesno Nothing)
  print (yesno (Just Nothing))
  print (yesno [3,4,5])
  print (yesno [])
  print (yesno (Just [3,4,5]))
  print (yesno (Just []))
