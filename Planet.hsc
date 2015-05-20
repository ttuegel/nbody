{-# LANGUAGE RecordWildCards #-}

module Planet where

import Foreign.ForeignPtr (newForeignPtr_)
import Foreign.Ptr
import Foreign.Storable (Storable(..))

import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as M

#include "nbody.h"

data Planet =
    Planet
    { pos :: {-# UNPACK #-} !(Vector Double)
    , vel :: {-# UNPACK #-} !(Vector Double)
    , mass :: !Double
    }
    -- the pointers are directly unpacked into this record constructor
    -- that's why the vectors are unpacked, but the scalar is merely strict

instance Storable Planet where
    sizeOf _ = (#size body)
    alignment _ = (#size double)
    peekElemOff p i = do
        fp <- newForeignPtr_ (castPtr p :: Ptr Double)
        body <- V.freeze (M.unsafeFromForeignPtr fp (8 * i) 8)
        return Planet
            { pos = V.slice 0 3 body
            , vel = V.slice 4 3 body
            , mass = body V.! 7
            }
    pokeElemOff p i Planet {..} = do
        fp <- newForeignPtr_ (castPtr p :: Ptr Double)
        let body = M.unsafeFromForeignPtr fp (8 * i) 8
        V.copy (M.slice 0 3 body) pos
        V.copy (M.slice 4 3 body) vel
        M.unsafeWrite body 7 mass
