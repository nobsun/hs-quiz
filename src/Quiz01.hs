{-
% ghci Quiz01
GHCi, version 8.6.3: http://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Quiz01           ( Quiz01.hs, interpreted )
Ok, one module loaded.
*Quiz01> 56 `と` 97 `と` 33 `の` 和
186 
*Quiz01> 56 `と` 97 `と` 33 `の` 積▽g
179256
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Quiz01 where

import Data.Function

type M a = (a -> a -> a) -> a

instance Show a => Show (M a) where
  show = show . ($ const)

instance Num a => Num (M a) where
  (+) = mkbop (+)
  (-) = mkbop (-)
  (*) = mkbop (*)
  abs = mkuop abs
  signum = mkuop signum
  fromInteger = const . fromInteger

mkbop :: (a -> a -> a) -> M a -> M a -> M a
mkbop bop m n = const (bop (m bop) (n bop))

mkuop :: (a -> a) -> M a -> M a
mkuop uop m = const (uop (m const))

と :: M a -> M a -> M a
と m n bop = bop (m bop) (n bop) 

の :: M a -> (M a -> M a) -> M a
の = (&)

infixl 1 `の`

和 :: Num a => M a -> M a
和 m = const (m (+))

積 :: Num a => M a -> M a
積 m = const (m (*))
