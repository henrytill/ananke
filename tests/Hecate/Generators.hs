module Hecate.Generators where

import Test.QuickCheck

genHex :: Gen Char
genHex = elements $ ['A'..'F'] ++ ['0'..'9']
