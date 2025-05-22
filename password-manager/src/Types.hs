module Types where

data User = User {
    username :: String,
    pin :: String,
    passwords :: [PasswordEntry]
} deriving (Show, Read)

data PasswordEntry = PasswordEntry {
    title :: String,
    serviceName :: String,
    encryptedPassword :: String
} deriving (Show, Read)