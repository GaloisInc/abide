module TestParams where

import Data.Word

import Abide.CTypes

import TestTypes

-- | This is a hack.  The idea is that if we put a unique, recognizable value
-- in for each parameter, we can search through the assembly/binary to figure
-- out where it was passed.  For now each is assumed to be a single byte
-- value.  For example, if a 64 bit integer is passed on the stack, we want to
-- look for the magic value in the first byte, followed by seven 0x00 bytes.
magicValues :: [Word64]
magicValues = [0x11, 0x22 ..]

int64s = zip (repeat CInt64) magicValues

floats = zip (repeat CFloat) magicValues

exePath :: String -> String
exePath name = "test/test-data/" ++ name ++ ".x86.exe"

-- | A simple test of a few different values in registers, including both ints
-- and floats.
regTest :: (FilePath, FnParamSpec)
regTest = ( exePath "reg", zip [CInt32, CInt8, CInt64, CFloat] magicValues )

-- | A test of just integer values on the stack
intStackTest :: (FilePath, FnParamSpec)
intStackTest = ( exePath "int-stack", take 10 int64s )

floatStackTest :: (FilePath, FnParamSpec)
floatStackTest = ( exePath "float-stack", take 12 floats )
