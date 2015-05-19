-- The Computer Language Benchmarks Game
-- http://benchmarksgame.alioth.debian.org/
--
-- Contributed by Branimir Maksimovic
-- Updated by Thomas Tuegel

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Foreign.Ptr
import Foreign.Storable
import System.Environment
import Text.Printf

import Data.Foldable
import qualified Data.Vector.Storable as V
import Data.Vector.Storable.Mutable (IOVector)
import qualified Data.Vector.Storable.Mutable as M
import qualified Data.Vector.Generic.Mutable as M (mstream)
import Data.Vector.Fusion.Stream.Monadic (Stream)
import qualified Data.Vector.Fusion.Stream.Monadic as S

main :: IO ()
main = do
    n <- getArgs >>= readIO.head :: IO Int
    planets <- V.unsafeThaw $ V.fromList initialConditions
    energy planets >>= printf "%.9f\n"
    S.mapM_ (\_ -> advance planets) (S.enumFromStepN (0::Int) 1 n)
    energy planets >>= printf "%.9f\n"

data Planet = Planet { x, y, z, vx, vy, vz, mass :: Double }

squared :: Double -> Double -> Double -> Double
{-# INLINE squared #-}
squared x y z = x * x + y * y + z * z

energy :: IOVector Planet -> IO Double
energy planets = do
    let kinetic Planet {..} = 0.5 * mass * squared vx vy vz
        potential a b =
            let dx = x a - x b
                dy = y a - y b
                dz = z a - z b
                r = sqrt $ squared dx dy dz
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
            let dx = x a - x b
                dy = y a - y b
                dz = z a - z b
                rSq = squared dx dy dz
                mag = dt / (rSq * sqrt rSq)
                dfb = mass b * mag
                a' = a { vx = vx a - dx * dfb
                       , vy = vy a - dy * dfb
                       , vz = vz a - dz * dfb
                       }
            M.unsafeWrite planets i a'
            let dfa = mass a * mag
                b' = b { vx = vx b + dx * dfa
                       , vy = vy b + dy * dfa
                       , vz = vz b + dz * dfa
                       }
            M.unsafeWrite planets j b'

        a <- M.unsafeRead planets i
        let a' = a { x = x a + dt * vx a
                   , y = y a + dt * vy a
                   , z = z a + dt * vz a
                   }
        M.unsafeWrite planets i a'

initialConditions :: [Planet]
initialConditions = [sun, jupiter, saturn, uranus, neptune]
  where
    planets = tail initialConditions

    px = foldl' (+) 0 (map (\Planet {..} -> vx * mass) planets)
    py = foldl' (+) 0 (map (\Planet {..} -> vy * mass) planets)
    pz = foldl' (+) 0 (map (\Planet {..} -> vz * mass) planets)

    sun =
        Planet
        { x = 0
        , y = 0
        , z = 0
        , vx = -px / solar_mass
        , vy = -py / solar_mass
        , vz = -pz / solar_mass
        , mass = solar_mass
        }

    jupiter =
        Planet
        { x = 4.84143144246472090e+00
        , y = -1.16032004402742839e+00
        , z = -1.03622044471123109e-01
        , vx = 1.66007664274403694e-03*dp
        , vy = 7.69901118419740425e-03*dp
        , vz = -6.90460016972063023e-05*dp
        , mass = 9.54791938424326609e-04 * solar_mass
        }

    saturn =
        Planet
        { x = 8.34336671824457987e+00
        , y = 4.12479856412430479e+00
        , z = -4.03523417114321381e-01
        , vx = -2.76742510726862411e-03*dp
        , vy = 4.99852801234917238e-03*dp
        , vz = 2.30417297573763929e-05*dp
        , mass = 2.85885980666130812e-04 * solar_mass
        }

    uranus =
        Planet
        { x = 1.28943695621391310e+01
        , y = -1.51111514016986312e+01
        , z = -2.23307578892655734e-01
        , vx = 2.96460137564761618e-03*dp
        , vy = 2.37847173959480950e-03*dp
        , vz = -2.96589568540237556e-05*dp
        , mass = 4.36624404335156298e-05 * solar_mass
        }

    neptune =
        Planet
        { x = 1.53796971148509165e+01
        , y = -2.59193146099879641e+01
        , z = 1.79258772950371181e-01
        , vx = 2.68067772490389322e-03*dp
        , vy = 1.62824170038242295e-03*dp
        , vz = -9.51592254519715870e-05*dp
        , mass = 5.15138902046611451e-05 * solar_mass
        }

days_per_year, dp, solar_mass, dt :: Double
days_per_year = 365.24
dp = days_per_year
solar_mass = 4 * pi * pi
dt = 0.01

instance Storable Planet where
    sizeOf _ = 8 * sizeOf (undefined::Double)
    alignment _ = sizeOf (undefined::Double)
    peekElemOff p i = peek (plusPtr p (i * sizeOf (undefined::Planet)))
    pokeElemOff p i e = poke (plusPtr p (i * sizeOf e)) e
    peek p = do
        x <- peek (offset 0)
        y <- peek (offset 1)
        z <- peek (offset 2)
        vx <- peek (offset 3)
        vy <- peek (offset 4)
        vz <- peek (offset 5)
        mass <- peek (offset 6)
        return $ Planet {x=x,y=y,z=z,vx=vx,vy=vy,vz=vz,mass=mass}
            where
                offset i = plusPtr (castPtr p::Ptr Double) (i*8)
    poke p e = do
        poke (offset 0) $ x e
        poke (offset 1) $ y e
        poke (offset 2) $ z e
        poke (offset 3) $ vx e
        poke (offset 4) $ vy e
        poke (offset 5) $ vz e
        poke (offset 6) $ mass e
            where
                offset i = plusPtr (castPtr p::Ptr Double) (i*8)
