module TestParams where

import Data.Word

import Abide.CTypes

-- This is a somewhat fragile hack.  The idea is that if we put a unique,
-- recognizable value in for each parameter, we can search through the
-- assembly/binary to figure out where it was passed.
magicValues :: [Word64]
magicValues = [0x11, 0x22 ..]

trivialTest :: (FilePath, [(CType, Word64)])
trivialTest = ( "test/test-data/simple.x86.exe"
              , zip [CInt32, CInt8, CInt64, CFloat] magicValues
              )

easyStackTest :: (FilePath, [(CType, Word64)])
easyStackTest = ( "test/test-data/stack.x86.exe"
                , zip (replicate 10 CInt64) magicValues
                )
