{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module Test where 

class TooMany a where 
    tooMany :: a -> Bool

instance TooMany Int where 
    tooMany n = n > 42

newtype Goats = Goats Int deriving (Eq, Show, TooMany)

newtype GoatsPair = GoatsPair (Int, Int) deriving TooMany

instance TooMany (Int, Int) where
    tooMany p = tooMany $ uncurry (+) p

--instance TooMany GoatsPair where
--    tooMany (GoatsPair p) = tooMany $ uncurry (+) p

instance TooMany (Int, String) where
    tooMany p = tooMany $ fst p
