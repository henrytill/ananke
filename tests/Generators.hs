module Generators where

import Hecate.Crypto (generateMasterKey)
import Hecate.Types
import Test.QuickCheck
import Instances ()

genHex :: Gen Char
genHex = elements $ ['A'..'F'] ++ ['0'..'9']

genMasterKey :: Gen MasterKey
genMasterKey = generateMasterKey <$> arbitrary <*> arbitrary
