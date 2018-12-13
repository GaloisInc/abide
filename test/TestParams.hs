module TestParams where

import Data.Word

import Abide.CTypes

-- This is a somewhat fragile hack.  The idea is that if we put a unique,
-- recognizable value in for each parameter, we can search through the
-- assembly/binary to figure out where it was passed.
magicValues :: [Word64]
magicValues = [0x11, 0x22 ..]

trivialParams :: (FilePath, [(CType, Word64)])
trivialParams = ("test/test-data/simple.x86.exe", [ (CInt32, 0x11), (CInt8, 0x22), (CInt64, 0x33), (CFloat, 0x44) ])
