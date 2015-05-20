-- The Computer Language Benchmarks Game
-- http://benchmarksgame.alioth.debian.org/
--
-- Contributed by Branimir Maksimovic
-- Updated by Thomas Tuegel

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Main (main) where

import Data.Monoid ((<>))
import System.Environment
import Text.Printf

import qualified Language.C.Inline as C

import Data.Foldable
import qualified Data.Vector.Storable as V
import Data.Vector.Storable.Mutable (IOVector)

import Planet

C.context (C.baseCtx <> C.vecCtx <> nbodiesCtx)

C.include "<math.h>"
C.include "nbody.h"

main :: IO ()
main = do
    n <- getArgs >>= readIO.head :: IO Int
    planets <- V.unsafeThaw $ V.fromList initialConditions
    energy planets >>= printf "%.9f\n"
    run n planets
    energy planets >>= printf "%.9f\n"

energy :: IOVector Planet -> IO Double
energy planets = fmap realToFrac
    [C.block| double
    {
        double energy = 0;
        body *planets = $vec-ptr:(body *planets);
        int i, j, k;

        /* Kinetic energy */
        for (i = 0; i < $vec-len:planets; i++) {
            double vv = 0;
            for (k = 0; k < 3; k++)
                vv += planets[i].v[k] * planets[i].v[k];

            energy += 0.5 * planets[i].mass * vv;
        }

        /* Potential energy */
        for (i = 0; i < $vec-len:planets; i++) {
            for (j = i + 1; j < $vec-len:planets; j++) {
                double rr = 0;
                for (k = 0; k < 3; k++) {
                    double dx = planets[i].x[k] - planets[j].x[k];
                    rr += dx * dx;
                }
                energy -= planets[i].mass * planets[j].mass / sqrt(rr);
            }
        }

        return energy;
    }
    |]

run :: Int -> IOVector Planet -> IO ()
run (fromIntegral -> steps) planets =
    [C.block| void
    {
        const int nplanets = $vec-len:planets;
        body *planets = $vec-ptr:(body *planets);
        const double dt = 0.01;
        int i, j, k, m, n;

        const int N = ((nplanets - 1) * nplanets) / 2;
        struct { double dx[3], fill; } r[N];
        double mag[N];

        for (n = 0; n < $(int steps); n ++) {

        /* update velocities */

        /* calculate displacement between each pair of planets */
        for (i = 0, k = 0; i < nplanets - 1; ++i) {
            for (j = i + 1; j < nplanets; ++j, ++k) {
                for (m = 0; m < 3; ++m)
                    r[k].dx[m] = planets[i].x[m] - planets[j].x[m];
            }
        }

        for (k = 0; k < N; ++k) {
            double rr = 0;
            for (m = 0; m < 3; ++m)
                rr += r[k].dx[m] * r[k].dx[m];
            mag[k] = dt / (rr * sqrt(rr));
        }

        for (i = 0, k = 0; i < nplanets - 1; ++i) {
            for (j = i + 1; j < nplanets; ++j, ++k) {
                const double magi = planets[j].mass * mag[k];
                for (m = 0; m < 3; ++m)
                    planets[i].v[m] -= r[k].dx[m] * magi;

                const double magj = planets[i].mass * mag[k];
                for (m = 0; m < 3; ++m)
                    planets[j].v[m] += r[k].dx[m] * magj;
            }
        }

        /* update positions */
        for (i = 0; i < nplanets; ++i) {
            for (m = 0; m < 3; ++m)
                planets[i].x[m] += dt * planets[i].v[m];
        }

        }
    }
    |]

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

days_per_year, dp, solar_mass :: Double
days_per_year = 365.24
dp = days_per_year
solar_mass = 4 * pi * pi
