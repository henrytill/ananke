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
  :: MonadError Error m
  => MasterKey
  -> Nonce
  -> Description
  -> PlainText
  -> m (CipherText, AuthTag)
encryptM (MasterKey mk) (Nonce nce) (Description d) (PlainText t) =
  let result      = encrypt (unByteString64 nce) (unByteString64 mk) (encodeUtf8 d) (encodeUtf8 t)
      f (ct, tag) = pure (CipherText (ByteString64 ct), AuthTag (ByteString64 tag))
  in onCryptoFailure (throwError . Crypto) f result

decrypt
  :: BS.ByteString
  -> BS.ByteString
  -> BS.ByteString
  -> BS.ByteString
  -> CryptoFailable (BS.ByteString, BS.ByteString)
decrypt nce k aad ciphertext = do
  st1 <- C.nonce12 nce >>= C.initialize k
  let st2        = C.finalizeAAD (C.appendAAD aad st1)
      (out, st3) = C.decrypt ciphertext st2
      tag        = C.finalize st3
  return (out, BA.convert tag)

decryptM
  :: MonadError Error m
  => MasterKey
  -> Nonce
  -> Description
  -> CipherText
  -> m (PlainText, AuthTag)
decryptM (MasterKey mk) (Nonce nce) (Description d) (CipherText t) =
  let result      = decrypt (unByteString64 nce) (unByteString64 mk) (encodeUtf8 d) (unByteString64 t)
      f (pt, tag) = pure (PlainText (decodeUtf8 pt), AuthTag (ByteString64 tag))
  in onCryptoFailure (throwError . Crypto) f result

generateKey :: T.Text -> BS.ByteString -> BS.ByteString
generateKey = generate (Parameters 16384 8 1 32) . encodeUtf8

generateMasterKey
  :: MasterPassword
  -> Salt
  -> MasterKey
generateMasterKey (MasterPassword pw) (Salt s) =
  MasterKey . ByteString64 . generate (Parameters 16384 8 1 32) (encodeUtf8 pw) $ unByteString64 s

genAuth :: MasterPassword -> Salt -> Auth
genAuth mp s = Auth { key = generateMasterKey mp s, salt = s }

ensureAuth :: MonadError Error m => MasterPassword -> Auth -> m MasterKey
ensureAuth mp a =
  if key a == generateMasterKey mp (salt a)
  then pure (key a)
  else throwError (AuthVerification "Can't re-generate MasterKey from given MasterPassword")
