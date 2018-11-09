-- Catalan families as Magmas
-- by Harald Bögeholz (harald.boegeholz@monash.edu)

{-
This code implements some of the ideas of the following paper [1]:
@article{brak2018universal,
  title={A Universal Bijection for Catalan Structures},
  author={Brak, Richard},
  journal={arXiv preprint arXiv:1808.09078},
  year={2018}
}
-}

data FreeMagma = Gen | FreeMagma :*: FreeMagma deriving (Show, Eq)

-- | a unique factorisation normed magma with one irreducible element
class (Eq a) => CatalanMagma a where

  -- | the unique irreducible element
  generator :: a

  -- | magma product map
  (.*.) :: a -> a -> a

  -- | full multiplication
  fromFree :: FreeMagma -> a
  fromFree Gen = generator
  fromFree (a :*: b) = fromFree a .*. fromFree b
  
  -- | enumerate all elements with given norm. Could be more efficient if
  -- it used more space to remember all elements with smaller norm.
  enumerate :: Int -> [a]
  enumerate n
    | n <= 1 = [generator]
    | otherwise = [ a .*. b
                  | i <- [1 .. n-1]
                  , a <- enumerate (n-i)
                  , b <- enumerate i
                  ]
                  
  norm :: a -> Int
  
  -- | decomposition. The default implementation is inefficient since it
  -- simply enumerates all elements with given norm until it finds the correct
  -- one.
  toFree :: a -> FreeMagma
  toFree x = head $ [y | (x', y) <- zip (enumerate n) (enumerate n)
                       , x' == x ]
    where n = norm x

-- --------------------------------------------------------------------------
-- | a free magma with one generator 'Gen' and product ':*:'
instance CatalanMagma FreeMagma where
  generator = Gen
  (.*.) = (:*:)
  fromFree = id

  norm Gen = 1
  norm (a :*: b) = norm a + norm b
    

-- --------------------------------------------------------------------------
-- | Dyck words. Matching pairs of brackets. $\F_1$ in [1].
newtype DyckWord = DyckWord String deriving (Eq, Show)

instance CatalanMagma DyckWord where
  generator = DyckWord ""
  DyckWord a .*. DyckWord b  = DyckWord $ a ++ "{" ++ b ++ "}"
  norm (DyckWord s) = length s `div` 2 + 1
  

-- --------------------------------------------------------------------------
-- | 321-avoiding permutations. $F_9$ in [1].
newtype PermAvoiding321 = PermAvoiding321 [Int] deriving (Eq, Show)

instance CatalanMagma PermAvoiding321 where
  generator = PermAvoiding321 []
  PermAvoiding321 xs .*. PermAvoiding321 ys = PermAvoiding321 $ xs ++ (i : ys')
    where p1 = length xs
          c1 = length $ takeWhile (\(a, b) -> a <= b) $ zip ys [1..]
          i = p1 + c1 + 1
          ys' = map shift ys
          shift y = p1 + y + if y>c1 then 1 else 0
  norm (PermAvoiding321 xs) = length xs + 1
       