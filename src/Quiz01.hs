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

newtype N a = N { unN :: (a -> a -> a) -> a }

instance Num a => Num (N a) where
  (+) = mknbo (+)
  (*) = mknbo (*)
  (-) = mknbo (-)
  abs = mknuo abs
  signum = mknuo signum
  fromInteger = N . const . fromInteger

mknbo :: (a -> a -> a) -> (N a -> N a -> N a)
mknbo f m n = N $ \ _ -> unN m f `f` unN n f

mknuo :: (a -> a) -> (N a -> N a)
mknuo f n = N $ \ g -> f (unN n g)
  
と :: N a -> N a -> N a
と f g = N $ \ h -> h (unN f h) (unN g h)

の :: N a -> (a -> a -> a) -> a
の = unN

infixr 0 `の`

和 :: Num a => a -> a -> a
和 = (+)

積 :: Num a => a -> a -> a
積 = (*)

newtype M a = M { unM :: [a] }

instance Num a => Num (M a) where
  (+) = mkmbo (+)
  (*) = mkmbo (*)
  (-) = mkmbo (-)
  abs = mkmuo abs
  signum = mkmuo signum
  fromInteger x = M [fromInteger x]
  
mkmbo :: (a -> a -> a) -> (M a -> M a -> M a)
mkmbo f m n = M [foldr1 f (unM m) `f` foldr1 f (unM n)]

mkmuo :: (a -> a) -> (M a -> M a)
mkmuo f = M . (f <$>) . unM

ト :: M a -> M a -> M a
ト x y = M $ unM x <> unM y

ノ :: M a -> (a -> a -> a) -> a
ノ = flip foldr1 . unM

infixr 0 `ノ`