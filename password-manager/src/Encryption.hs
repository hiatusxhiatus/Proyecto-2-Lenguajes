module Encryption (encryptPassword, decryptPassword, padKey, pad, unpad) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Base16 as B16
import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (cipherInit, ecbEncrypt, ecbDecrypt, BlockCipher(..))
import Crypto.Error (throwCryptoError, CryptoFailable)

padKey :: String -> B.ByteString
padKey key = let k = BC.pack key in B.take 32 (k <> B.replicate 32 0)

pad :: B.ByteString -> B.ByteString
pad bs = let len = B.length bs
             padLen = 16 - (len `mod` 16)
         in bs <> B.replicate padLen (fromIntegral padLen)

unpad :: B.ByteString -> B.ByteString
unpad bs = let len = B.length bs
               padLen = fromIntegral (B.last bs)
           in B.take (len - padLen) bs

encryptPassword :: String -> String -> String
encryptPassword key plainText =
    let keyBytes = padKey key
        plainBytes = BC.pack plainText
        cipher = throwCryptoError (cipherInit keyBytes :: CryptoFailable AES256)
        encrypted = ecbEncrypt cipher (pad plainBytes)
    in BC.unpack $ B16.encode encrypted

decryptPassword :: String -> String -> String
decryptPassword key encryptedText =
    let keyBytes = padKey key
        encryptedBytes = case B16.decode $ BC.pack encryptedText of
            Right bs -> bs
            Left err -> error $ "Error decodificando base16: " ++ err
        cipher = throwCryptoError (cipherInit keyBytes :: CryptoFailable AES256)
        decrypted = ecbDecrypt cipher encryptedBytes
    in BC.unpack $ unpad decrypted
