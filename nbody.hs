-- The Computer Language Benchmarks Game
-- http://benchmarksgame.alioth.debian.org/
--
-- Contributed by Branimir Maksimovic
-- Updated by Thomas Tuegel

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import System.Environment
import Text.Printf

import Data.Foldable
import Data.Vector.Storable (Storable, Vector)
import qualified Data.Vector.Storable as V
import Data.Vector.Storable.Mutable (IOVector)
import qualified Data.Vector.Storable.Mutable as M
import qualified Data.Vector.Generic.Mutable as M (mstream)
import Data.Vector.Fusion.Stream.Monadic (Stream)
import qualified Data.Vector.Fusion.Stream.Monadic as S

import Planet

main :: IO ()
main = do
    n <- getArgs >>= readIO.head :: IO Int
    planets <- V.unsafeThaw $ V.fromList initialConditions
    energy planets >>= printf "%.9f\n"
    S.mapM_ (\_ -> advance planets) (S.enumFromStepN (0::Int) 1 n)
    energy planets >>= printf "%.9f\n"

squared :: Vector Double -> Double
{-# INLINE squared #-}
squared = V.sum . V.map (\x -> x * x)

energy :: IOVector Planet -> IO Double
energy planets = do
    let kinetic Planet {..} = 0.5 * mass * squared vel
        potential a b =
            let dx = V.zipWith (-) (pos a) (pos b)
                r = sqrt $ squared dx
            in negate $ mass a * mass b / r
    totalKE <- S.foldl' (+) 0 (S.map kinetic (M.mstream planets))
    totalPE <- S.foldl' (+) 0 (pairwise potential planets)
    return (totalKE + totalPE)

for :: Monad m => (a, a -> Bool, a -> a) -> (a -> m ()) -> m ()
{-# INLINE for #-}
for = \(a0, check, next) act ->
    let for_loop a
          | check a = act a >> for_loop (next a)
          | otherwise = return ()
    in for_loop a0

pairwise :: Storable a => (a -> a -> b) -> IOVector a -> Stream IO b
pairwise f v =
    let len = M.length v
        ixs = S.concatMap
              (\i -> S.map ((,) i) (S.enumFromStepN (i + 1) 1 (len - i - 1)))
              (S.enumFromStepN 0 1 len)
    in S.mapM (\(i, j) -> f <$> M.unsafeRead v i <*> M.unsafeRead v j) ixs

advance :: IOVector Planet -> IO ()
{-# INLINE advance #-}
advance planets = do
    let nbodies = M.length planets
    for (0, (< nbodies), (+ 1)) $ \i -> do
        for (i + 1, (< nbodies), (+ 1)) $ \j -> do
            a <- M.unsafeRead planets i
            b <- M.unsafeRead planets j
            let dx = V.zipWith (-) (pos a) (pos b)
                rSq = squared dx
                mag = dt / (rSq * sqrt rSq)
                dfb = negate (mass b * mag)
                a' = a { vel = V.zipWith (+) (vel a) (V.map (* dfb) dx) }
            M.unsafeWrite planets i a'
            let dfa = mass a * mag
                b' = b { vel = V.zipWith (+) (vel b) (V.map (* dfa) dx) }
            M.unsafeWrite planets j b'

        a <- M.unsafeRead planets i
        let a' = a { pos = V.zipWith (+) (pos a) (V.map (* dt) (vel a)) }
        M.unsafeWrite planets i a'

initialConditions :: [Planet]
initialConditions = [sol, jupiter, saturn, uranus, neptune]
  where
    planets = tail initialConditions

    -- initial momentum of the Jovian planets
    -- Sol's initial momentum is set so the system's total momentum is zero
    poff = foldl' (V.zipWith (+)) (V.replicate 3 0)
           $ map (\Planet {..} -> V.map (* mass) vel) planets

    sol =
        Planet
        { pos = V.fromList [0, 0, 0]
        , vel = V.map (negate . (/ solar_mass)) poff
        , mass = solar_mass
        }

    jupiter =
        Planet
        { pos = V.fromList
            [ 4.84143144246472090e+00
            , -1.16032004402742839e+00
            , -1.03622044471123109e-01
            ]
        , vel = V.fromList
            [ 1.66007664274403694e-03*dp
            , 7.69901118419740425e-03*dp
            , -6.90460016972063023e-05*dp
            ]
        , mass = 9.54791938424326609e-04 * solar_mass
        }

    saturn =
        Planet
        { pos = V.fromList
            [ 8.34336671824457987e+00
            , 4.12479856412430479e+00
            , -4.03523417114321381e-01
            ]
        , vel = V.fromList
            [ -2.76742510726862411e-03*dp
            , 4.99852801234917238e-03*dp
            , 2.30417297573763929e-05*dp
            ]
        , mass = 2.85885980666130812e-04 * solar_mass
        }

    uranus =
        Planet
        { pos = V.fromList
            [ 1.28943695621391310e+01
            , -1.51111514016986312e+01
            , -2.23307578892655734e-01
            ]
        , vel = V.fromList
            [ 2.96460137564761618e-03*dp
            , 2.37847173959480950e-03*dp
            , -2.96589568540237556e-05*dp
            ]
        , mass = 4.36624404335156298e-05 * solar_mass
        }

    neptune =
        Planet
        { pos = V.fromList
            [ 1.53796971148509165e+01
            , -2.59193146099879641e+01
            , 1.79258772950371181e-01
            ]
        , vel = V.fromList
            [ 2.68067772490389322e-03*dp
            , 1.62824170038242295e-03*dp
            , -9.51592254519715870e-05*dp
            ]
        , mass = 5.15138902046611451e-05 * solar_mass
        }

days_per_year, dp, solar_mass, dt :: Double
days_per_year = 365.24
dp = days_per_year
solar_mass = 4 * pi * pi
dt = 0.01
