-- Catalan families as Magmas
-- by Harald Bögeholz (harald.boegeholz@monash.edu)

data FreeMagma = Gen | FreeMagma :*: FreeMagma deriving (Show, Eq)

class CatalanMagma a where

  generator :: a

  (.*.) :: a -> a -> a

  fromFree :: FreeMagma -> a
  fromFree Gen = generator
  fromFree (a :*: b) = fromFree a .*. fromFree b
  
  -- norm :: a -> Int

instance CatalanMagma FreeMagma where
  generator = Gen
  (.*.) = (:*:)
  fromFree = id

-- --------------------------------------------------------------------------
newtype DyckMagma = DyckMagma String deriving (Eq, Show)
instance CatalanMagma DyckMagma where
  generator = DyckMagma ""
  DyckMagma a .*. DyckMagma b  = DyckMagma $ a ++ "(" ++ b ++ ")"
  

-- --------------------------------------------------------------------------
newtype PermAvoiding321 = PermAvoiding321 [Int] deriving (Eq, Show)

instance CatalanMagma PermAvoiding321 where
  generator = PermAvoiding321 []
  PermAvoiding321 xs .*. PermAvoiding321 ys = PermAvoiding321 $ xs ++ (i : ys')
    where p1 = length xs
          c1 = length $ takeWhile (\(a, b) -> a > b) $ zip ys [1..]
          i = p1 + c1 + 1
          ys' = map shift ys
          shift y = p1 + y + if y>c1 then 1 else 0
          