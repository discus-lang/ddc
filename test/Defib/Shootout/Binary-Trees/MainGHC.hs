{-# OPTIONS_GHC -fglasgow-exts -O2 -optc-O3 -funbox-strict-fields #-}
-- The Great Computer Language Shootout
-- http://shootout.alioth.debian.org/
-- Simon Marlow
-- Shortened by Don Stewart

import System; import Text.Printf; import Monad

data Tree = Nil | Node !Int Tree Tree

min' = 4 :: Int

main = do max' <- getArgs >>= return . max (min'+2) . read . head
          printf "stretch tree of depth %d\t check: %d\n" (max'+1) (itemCheck $ make 0 (max'+1))
          depthLoop min' max'
          printf "long lived tree of depth %d\t check: %d\n" max' (itemCheck $ make 0 max')

depthLoop d m = when (d <= m) $ do
    printf "%d\t trees of depth %d\t check: %d\n" (2*n) d (sumLoop n d 0)
    depthLoop (d+2) m
    where n = 2^(m - d + min')

sumLoop 0 d acc = acc
sumLoop k d acc = c `seq` sumLoop (k-1) d (acc + c + c')
    where (c,c')  = (itemCheck (make k d), itemCheck (make (-1*k) d))

make i (0::Int) = i `seq` Nil
make i  d       = Node i (make ((2*i)-1) (d-1)) (make (2*i) (d-1))

itemCheck Nil = 0
itemCheck (Node x l r) = x + itemCheck l - itemCheck r

