
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS -fvia-C -O2 -optc-O2 -optc-ffast-math -fbang-patterns -fexcess-precision -funbox-strict-fields #-}
--
-- The Computer Language Benchmarks Game
-- http://shootout.alioth.debian.org/
--
-- Contributed by Don Stewart
-- A lazy bytestring solution.
-- Unnecessary strictness annotations removed by Sterling Clover 2/08
--
-- Adding cached version of the list, where the first 4 entries are
-- lifted into the data constructor by Scott West 03/10
--
-- Add:
-- -optc-mfpmath=sse -optc-msse2
--
module Main where

import System
import Data.Word
import Control.Arrow

import Data.List

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C (pack)
import qualified Data.ByteString as S
import Data.ByteString.Internal

main = do
    n <- getArgs >>= readIO . head
    writeFasta  "ONE"   "Homo sapiens alu"       (n*2) (L.cycle alu)
    g <- unfold "TWO"   "IUB ambiguity codes"    (n*3) (mkCacher $ cdfize iubs) 42
    unfold      "THREE" "Homo sapiens frequency" (n*5) (mkCacher $ cdfize homs) g

------------------------------------------------------------------------
--
-- lazily unfold the randomised dna sequences
--

unfold lab ttl n probs gen =
    putStrLn (">" ++ lab ++ " " ++ ttl) >> unroll probs gen n

unroll :: Cacher -> Int -> Int -> IO Int
unroll probs = loop
    where
        loop r 0   = return r
        loop !r i = case S.unfoldrN m (Just . look probs) r of
                        (!s, Just r') -> do
                            S.putStrLn s
                            loop r' (i-m)
          where m = min i 60

look cr k = (choose cr d, j)
  where R d j = rand k

-- Chunk the list into parts, still can represent any list of the
-- symbol/probability pairs.
data PPair = PPair !Word8 !Float
data Cacher = Cacher !PPair !PPair !PPair !PPair [PPair]
            | CacheList ![PPair]

mkCacher (p1:p2:p3:p4:ds) = Cacher p1 p2 p3 p4 ds
mkCacher ds = CacheList ds

cdfize :: [(Word8,Float)] -> [PPair]
cdfize ds = init cdf' ++ [PPair s 1.0]
    where
      PPair s _ = last cdf'
      cdf' = (map (uncurry PPair) . snd . mapAccumL go 0) ds
      go c (sym, prob) = (c + prob, (sym, c+prob))

-- We still query the list in order, but we don't have to continually
-- ``uncons'' everything, we can do the first 4 at a time.
choose :: Cacher -> Float -> Word8
choose (Cacher (PPair s1 c1) (PPair s2 c2) (PPair s3 c3) (PPair s4 c4) ds) p
    | p <= c1 = s1
    | p <= c2 = s2
    | p <= c3 = s3
    | p <= c4 = s4
    | otherwise = chooseCdf ds p
                  {- case ds of
                    CacheList [] -> s4
                    _ -> choose ds p -}
choose (CacheList ds) p = chooseCdf ds p

chooseCdf :: [PPair] -> Float -> Word8
chooseCdf ((PPair b f):xs) p = if p < f then b else chooseCdf xs p

------------------------------------------------------------------------
--
-- only demand as much of the infinite sequence as we require

writeFasta label title n s = do
     putStrLn $ ">" ++ label ++ " " ++ title
     let (t:ts) = L.toChunks s
     go ts t n
  where
     go ss s n
        | l60 && n60 = S.putStrLn l               >> go ss        r (n-60)
        |        n60 = S.putStr s >> S.putStrLn a >> go (tail ss) b (n-60)
        | n <= ln    = S.putStrLn (S.take n s)
        | otherwise  = S.putStr s >> S.putStrLn (S.take (n-ln) (head ss))
        where
            ln   = S.length s
            l60  = ln >= 60
            n60  = n  >= 60
            (l,r) = S.splitAt 60 s
            (a,b) = S.splitAt (60-ln) (head ss)

------------------------------------------------------------------------
im, ia, ic :: Int
im  = 139968
ia  = 3877
ic  = 29573

data R = R !Float !Int deriving Show

rand :: Int -> R
rand seed = R newran newseed
    where
        newseed = (seed * ia + ic) `rem` im
        newran  =  1.0 * fromIntegral newseed / imd
        imd      = fromIntegral im

------------------------------------------------------------------------

alu = C.pack
        "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG\
        \GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA\
        \CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT\
        \ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA\
        \GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG\
        \AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC\
        \AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"

iubs, homs :: [(Word8, Float)]
iubs = map (c2w *** id)
        [('a',0.27),('c',0.12),('g',0.12),('t',0.27),('B',0.02)
        ,('D',0.02),('H',0.02),('K',0.02),('M',0.02),('N',0.02)
        ,('R',0.02),('S',0.02),('V',0.02),('W',0.02),('Y',0.02)]

homs = map (c2w *** id)
        [('a',0.3029549426680),('c',0.1979883004921)
        ,('g',0.1975473066391),('t',0.3015094502008)]
