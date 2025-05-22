module Storage (saveUserData, loadUserData, getHiddenInput, getVisibleInput, loadAllUsers, saveAllUsers, userExists) where

import System.Directory (doesFileExist)
import System.IO (hSetEcho, stdin, hFlush, stdout)
import Types
import Control.Exception (catch, SomeException)
import Data.List (find)
import Encryption (encryptPassword)

-- Archivo donde se almacenan todos los usuarios
usersFile :: String
usersFile = "users.enc"

-- Función para cargar todos los usuarios del sistema
loadAllUsers :: IO [User]
loadAllUsers = do
    exists <- doesFileExist usersFile
    if not exists 
        then return []
        else catch tryRead handler
  where
    tryRead = do
        content <- readFile usersFile
        if null content
            then return []
            else case reads content of
                [(users, "")] -> return users
                _ -> return []
    handler :: SomeException -> IO [User]
    handler _ = return []

-- Función para guardar todos los usuarios
-- Modifica la función saveAllUsers así:
saveAllUsers :: [User] -> IO ()
saveAllUsers users = do
    exists <- doesFileExist usersFile
    if not exists
        then writeFile usersFile (show ([] :: [User])) 
        else return ()
    writeFile usersFile (show users)

loadUserData :: String -> String -> IO (Maybe User)
loadUserData inputUsername inputPin = do
    users <- loadAllUsers
    case find (\u -> username u == inputUsername) users of
        Nothing -> return Nothing
        Just user -> 
            -- Comparar el PIN ingresado con el PIN encriptado almacenado
            if encryptPassword inputPin inputPin == pin user
                then return $ Just user
                else return Nothing

saveUserData :: User -> IO ()
saveUserData user = do
    users <- loadAllUsers
    let updatedUsername = username user
        -- Asegurarse de que el PIN está encriptado antes de guardar
        encryptedUser = if isEncrypted (pin user)
                        then user
                        else user { pin = encryptPassword (pin user) (pin user) }
        otherUsers = filter (\u -> username u /= updatedUsername) users
        newUsers = encryptedUser : otherUsers
    saveAllUsers newUsers
  where
    -- Función simple para verificar si un PIN ya está encriptado
    isEncrypted :: String -> Bool
    isEncrypted s = length s > 16  -- Los PINs encriptados son más largos

-- Función para verificar si un username ya existe
userExists :: String -> IO Bool
userExists inputUsername = do
    users <- loadAllUsers
    return $ any (\u -> username u == inputUsername) users

-- Función para entrada oculta (para PINs y contraseñas)
getHiddenInput :: IO String
getHiddenInput = do
    hSetEcho stdin False
    input <- getLine
    hSetEcho stdin True
    return input

-- Función para entrada visible (para usuarios y títulos)
getVisibleInput :: IO String
getVisibleInput = do
    hFlush stdout
    getLine
