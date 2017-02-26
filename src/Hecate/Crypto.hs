{-# LANGUAGE FlexibleContexts #-}

module Hecate.Crypto where

import Control.Monad.Except
import Crypto.Error (CryptoFailable, onCryptoFailure)
import Crypto.KDF.Scrypt (Parameters (..), generate)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Hecate.Types
import qualified Crypto.Cipher.ChaChaPoly1305 as C
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.Text as T

encrypt
  :: BS.ByteString                                 -- nonce (12 random bytes)
  -> BS.ByteString                                 -- symmetric key
  -> BS.ByteString                                 -- additional data
  -> BS.ByteString                                 -- input plaintext to be encrypted
  -> CryptoFailable (BS.ByteString, BS.ByteString) -- (ciphertext, 128-bit tag)
encrypt nce k aad plaintext = do
  st1 <- C.nonce12 nce >>= C.initialize k
  let st2        = C.finalizeAAD (C.appendAAD aad st1)
      (out, st3) = C.encrypt plaintext st2
      tag        = C.finalize st3
  return (out, BA.convert tag)

encryptM
  :: MonadError AppError m
  => MasterKey
  -> Nonce
  -> Description
  -> Plaintext
  -> m (Ciphertext, AuthTag)
encryptM (MasterKey mk) (Nonce nce) (Description d) (Plaintext t) =
  let result      = encrypt (unByteString64 nce) (unByteString64 mk) (encodeUtf8 d) (encodeUtf8 t)
      f (ct, tag) = pure (Ciphertext (ByteString64 ct), AuthTag (ByteString64 tag))
  in onCryptoFailure (throwError . Crypto) f result

decrypt
  :: BS.ByteString
  -> BS.ByteString
  -> BS.ByteString
  -> BS.ByteString
  -> CryptoFailable (BS.ByteString, BS.ByteString)
decrypt nce k aad ct = do
  st1 <- C.nonce12 nce >>= C.initialize k
  let st2        = C.finalizeAAD (C.appendAAD aad st1)
      (out, st3) = C.decrypt ct st2
      tag        = C.finalize st3
  return (out, BA.convert tag)

decryptM
  :: MonadError AppError m
  => MasterKey
  -> Nonce
  -> Description
  -> Ciphertext
  -> m (Plaintext, AuthTag)
decryptM (MasterKey mk) (Nonce nce) (Description d) (Ciphertext t) =
  let result      = decrypt (unByteString64 nce) (unByteString64 mk) (encodeUtf8 d) (unByteString64 t)
      f (pt, tag) = pure (Plaintext (decodeUtf8 pt), AuthTag (ByteString64 tag))
  in onCryptoFailure (throwError . Crypto) f result

generateKey :: T.Text -> BS.ByteString -> BS.ByteString
generateKey = generate (Parameters 16384 8 1 32) . encodeUtf8

generateMasterKey :: MasterPassword -> Salt -> MasterKey
generateMasterKey (MasterPassword pw) (Salt s) =
  MasterKey . ByteString64 . generate (Parameters 16384 8 1 32) (encodeUtf8 pw) $ unByteString64 s