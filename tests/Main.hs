{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Crypto.Error (CryptoFailable (..))
import Data.Text.Encoding
import Hecate.Crypto
import System.Exit
import Test.QuickCheck
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary
  shrink xs = T.pack <$> shrink (T.unpack xs)

instance Arbitrary BS.ByteString where
  arbitrary = BS.pack <$> arbitrary
  shrink xs = BS.pack <$> shrink (BS.unpack xs)

roundTrip
  :: T.Text
  -> BS.ByteString
  -> BS.ByteString
  -> BS.ByteString
  -> T.Text
  -> Bool
roundTrip password salt nonce aad text =
  let key               = generatePassword password salt
      encodedText       = encodeUtf8 text
      (encrypted, etag) = case encrypt nonce key aad encodedText of
                            CryptoFailed e      -> error (show e)
                            CryptoPassed (c, t) -> (Base64.encode c, t)
      (decrypted, dtag) = case Base64.decode encrypted of
                            Left e   -> error (show e)
                            Right bs -> case decrypt nonce key aad bs of
                              CryptoFailed e      -> error (show e)
                              CryptoPassed (d, t) -> (decodeUtf8 d, t)
  in decrypted == text && dtag == etag

prop_roundTrip :: Property
prop_roundTrip =
  forAll arbitrary $ \password ->
  forAll arbitrary $ \salt     ->
  forAll arbNonce  $ \nonce    ->
  forAll arbitrary $ \aad      ->
  forAll arbitrary $ \text     ->
  roundTrip password salt nonce aad text
  where
    arbNonce = C.pack <$> sequence (take 12 (repeat arbitrary))

tests :: [Property]
tests = [prop_roundTrip]

main :: IO ()
main =
  mapM (quickCheckWithResult stdArgs) tests >>= pure . all isSuccess >>= exit
  where
    isSuccess :: Result -> Bool
    isSuccess (Success _ _ _) = True
    isSuccess _               = False
    exit :: Bool -> IO ()
    exit True  = exitSuccess
    exit False = exitFailure
