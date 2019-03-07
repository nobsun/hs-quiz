{-
% ghci Quiz01
GHCi, version 8.6.3: http://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Quiz01           ( Quiz01.hs, interpreted )
Ok, one module loaded.
*Quiz01> 56 `と` 97 `と` 33 `の` 和
186 
*Quiz01> 56 `と` 97 `と` 33 `の` 積
179256
-}
module Quiz01 where

newtype N a = N {unN :: [a]}

instance Show a => Show (N a) where
  show (N xs) = if singleton xs then show $ head xs else show xs
    where
      singleton [_] = True
      singleton _   = False

instance Num a => Num (N a) where
  (+) = mkmbo (+)
  (*) = mkmbo (*)
  (-) = mkmbo (-)
  abs = mkmuo abs
  signum = mkmuo signum
  fromInteger x = N [fromInteger x]
  
mkmbo :: (a -> a -> a) -> (N a -> N a -> N a)
mkmbo f m n = N [foldr1 f (unN m) `f` foldr1 f (unN n)]

mkmuo :: (a -> a) -> (N a -> N a)
mkmuo f = N . (f <$>) . unN

と :: N a -> N a -> N a
と m n = N $ unN m <> unN n

の :: N a -> (N a -> N a) -> N a
の = flip ($)

aggr :: (a -> a -> a) -> (N a -> N a)
aggr f n = N [foldl1 f (unN n)]

和 :: Num a => N a -> N a
和 = aggr (+)

積 :: Num a => N a -> N a
積 = aggr (*)
