module Hecate.Crypto where

import Crypto.Error (CryptoFailable)
import Crypto.KDF.Scrypt (Parameters (..), generate)
import Data.Text.Encoding (encodeUtf8)
import qualified Crypto.Cipher.ChaChaPoly1305 as C
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text as T

encrypt
  :: BS.ByteString                                 -- nonce (12 random bytes)
  -> BS.ByteString                                 -- symmetric key
  -> BS.ByteString                                 -- additional data
  -> BS.ByteString                                 -- input plaintext to be encrypted
  -> CryptoFailable (BS.ByteString, BS.ByteString) -- (ciphertext, 128-bit tag)
encrypt nonce key aad plaintext = do
  st1 <- C.nonce12 nonce >>= C.initialize key
  let st2        = C.finalizeAAD (C.appendAAD aad st1)
      (out, st3) = C.encrypt plaintext st2
      tag        = C.finalize st3
  return (out, BA.convert tag)

decrypt
  :: BS.ByteString
  -> BS.ByteString
  -> BS.ByteString
  -> BS.ByteString
  -> CryptoFailable (BS.ByteString, BS.ByteString)
decrypt nonce key aad ciphertext = do
  st1 <- C.nonce12 nonce >>= C.initialize key
  let st2        = C.finalizeAAD (C.appendAAD aad st1)
      (out, st3) = C.decrypt ciphertext st2
      tag        = C.finalize st3
  return (out, BA.convert tag)

generatePassword :: T.Text -> BS.ByteString -> BS.ByteString
generatePassword password = generate (Parameters 16384 8 1 32) (encodeUtf8 password)

recoverPassword :: T.Text -> BS.ByteString -> BS.ByteString
recoverPassword password salt = either error (generatePassword password) (Base64.decode salt)
