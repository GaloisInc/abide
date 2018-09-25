module Abide.CTypes where

import Numeric.Natural

-- Currently this module is not used.  Ultimately we will need to track the
-- size of all the relevant C types in order to compute the proper stack
-- offsets.

-- We don't care about signed/unsigned distinction since we only need the
-- number of bits.
data CType
  = CBool | CChar | CShort | CInt | CLong | CLongLong | CPointer
  | CInt8_t | CInt16_t | CInt32_t | CInt64_t
  | CFloat | CDouble | CDecimal32 | CDecimal64 
  | CFloat128 | CDecimal128 

-- This is for x86_64, arch should be factored out though and use a type class
-- to handle everything
ctypeSize :: CType -> Natural
ctypeSize CBool       = 1
ctypeSize CChar       = 1
ctypeSize CShort      = 2
ctypeSize CInt        = 4
ctypeSize CLong       = 8
ctypeSize CLongLong   = 8
ctypeSize CPointer    = 8
ctypeSize CInt8_t     = 1
ctypeSize CInt16_t    = 2
ctypeSize CInt32_t    = 4
ctypeSize CInt64_t    = 8
ctypeSize CFloat      = 4
ctypeSize CDouble     = 8
ctypeSize CDecimal32  = 4
ctypeSize CDecimal64  = 8
ctypeSize CFloat128   = 16
ctypeSize CDecimal128 = 16
