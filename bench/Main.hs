{-# LANGUAGE BangPatterns #-}
module Main where

import           Control.Monad
import           Control.Monad.ST
import           Criterion.Main
import qualified Data.Vector         as V
import qualified Data.Vector.Mutable as M

initV :: V.Vector Int
initV = V.fromList [0..10^4]

test1 :: Int -> V.Vector Int -> Int
test1 m v = runST $ do
  mv <- V.thaw v
  forM [0..m] (\i ->
      M.write mv i (i*i)
    )
  nv <- V.freeze mv
  return $ V.sum (V.reverse nv)


test1P :: Int -> V.Vector Int -> Int
test1P m v = V.sum nv
  where
    nv = for m v

    for (-1) v = v
    for n v    = (for (n-1) v) V.// [(n, n*n)]


test1M :: Int -> V.Vector Int -> Int
test1M m v = V.sum nv
  where
    nv = for m v
    set i mv = M.write mv i (i*i)
    for (-1) v = v
    for n v    = V.modify (set n) $ for (n-1) v

test1M' :: Int -> V.Vector Int -> Int
test1M' m v = V.sum nv
  where
    nv = loop 0 v
    loop k v'
      | k <= m = loop (k + 1) $! V.modify (\mv -> M.write mv k (k*k)) v'
      | otherwise = v'

test1M'' :: Int -> V.Vector Int -> Int
test1M'' m v = V.sum nv
  where
    nv = squareAll m v

squareAll :: Int -> V.Vector Int -> V.Vector Int
squareAll m v = go 0 v
  where
    n = length v
    go i v'
      | i < m     = go (i+1) $ V.modify (\mv -> M.write mv i (i*i)) v'
      | otherwise = v'

main :: IO ()
main = do
  print initV
  defaultMain [
    bgroup "sum of squares"
      [ bench "ST"                      $ whnf (test1     (10^2))  initV
      , bench "bulk update"             $ whnf (test1P    (10^2))  initV
      , bench "modify"                  $ whnf (test1M    (10^2))  initV
      , bench "modify (with $!)"        $ whnf (test1M'   (10^2))  initV
      , bench "modify (with squareAll)" $ whnf (test1M''  (10^2))  initV
      ]
    ]
