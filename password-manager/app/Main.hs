module Main where

import Storage
import Types
import UI
import Utils
import System.IO (hFlush, stdout)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import Control.Monad (unless)

-- Definiciones de colores ANSI
cyan :: String
cyan = "\ESC[36m"
bold :: String
bold = "\ESC[1m"
green :: String
green = "\ESC[32m"
red :: String
red = "\ESC[31m"
yellow :: String
yellow = "\ESC[33m"
magenta :: String
magenta = "\ESC[35m"
reset :: String
reset = "\ESC[0m"

-- ASCII Art para el administrador de contraseñas
mostrarASCII :: IO ()
mostrarASCII = do
    putStrLn $ cyan ++
        "      ⠀⢠⣄⠀⠀⠀⠀⠀⠀⣀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀\n" ++
        "      ⠀⢘⣾⣷⣤⣤⣤⣤⣴⢟⣾⠂⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀Lenguajes!⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀\n" ++
        "       ⠀⠨⣿⣿⢿⣿⢟⣯⢷⡟⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀\n" ++
        "       ⠀⠈⣿⣳⣾⣭⡸⢰⡻⢿⣦⣤⣤⣶⣶⣾⣿⣿⣿⣿⣶⣶⣄⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀\n" ++
        "       ⠀⠀⠙⢟⠛⡾⠡⠉⣲⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣷⣄⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀\n" ++
        "       ⠀⠀⠀⠀⢱⣆⣴⣾⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣷⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀\n" ++
        "       ⠀⠀⠀⠀⠀⢹⣷⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣷⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀\n" ++
        "       ⠀⠀⠀⠀⠀⠸⣹⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣇⠀⠀⠀⠀⠀⠀⠀⠀⢀⡀⠀⠀\n" ++
        "       ⠀⠀⠀⠀⠀⠀⠙⠿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⢿⣿⣿⣿⡀⠀⠀⠀⠀⣠⣴⣾⣿⡿⠂⠀\n" ++
        "       ⠀⠀⠀⠀⠀⠀⠀⠀⠘⢿⣿⣿⣿⣿⣿⢿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣷⣄⣠⣴⣿⣿⣿⠟⠋⠀⠀⠀\n" ++
        "       ⠀⠀⠀⠀⠀⠀⠀⠀⠀⣾⣿⣿⣿⣿⡏⠀⠻⣿⣿⣿⣿⣿⣿⣿⣿⣿⠙⢿⣿⣿⣿⣿⣿⣿⠿⠁⠀⠀⠀⠀⠀\n" ++
        "       ⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⣿⣿⣿⣿⡇⠀⠀⠈⠙⣿⣿⣿⢿⣿⣿⣿⠂⠀⠉⠛⠛⠛⠋⠁⠀⠀⠀⠀⠀⠀⠀\n" ++
        "       ⠀⠀⠀⠀⠀⠀⠀⠀⠀⣿⣿⡿⣿⣿⡇⠀⠀⠀⠀⢼⣿⡏⢸⣿⣿⡟⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀\n" ++
        "       ⠀⠀⠀⠀⠀⠀⠀⠀⠀⠿⠟⠀⣿⣿⠀⠀⠀⠀⠀⣿⣿⣧⣾⣿⡟⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀" ++ reset

-- Función para inicializar el sistema de archivos
initializeSystem :: IO ()
initializeSystem = do
    exists <- doesFileExist "users.enc"
    unless exists $ do
        writeFile "users.enc" (show ([] :: [User]))
        putStrLn $ green ++ "✓ Sistema de almacenamiento inicializado" ++ reset

main :: IO ()
main = do
    initializeSystem  -- Asegura que el archivo exista antes de continuar
    clearScreen
    mostrarASCII
    putStrLn $ cyan ++ bold ++ "\n      ╔════════════════════════════════════════╗" ++ reset
    putStrLn $ cyan ++ bold ++ "      ║  " ++ magenta ++ "ADMINISTRADOR DE CONTRASEÑAS SEGURAS" ++ cyan ++ bold ++ "  ║" ++ reset
    putStrLn $ cyan ++ bold ++ "      ╚════════════════════════════════════════╝" ++ reset
    
    -- Verificar si ya existe un usuario registrado
    exists <- doesFileExist "users.enc"
    
    putStrLn $ yellow ++ bold ++ "\nOpciones de acceso:" ++ reset
    putStrLn $ "  1. Acceder con usuario existente"
    putStrLn $ "  2. Crear nuevo usuario " ++ reset
    putStrLn $ "  3. Salir"

    putStr $ yellow ++ bold ++ "\n> Seleccione una opción (1-3): " ++ reset
    hFlush stdout
    choice <- getLine
    
    case choice of
        "1" -> if exists
                then loginUser
                else do
                    putStrLn $ red ++ "No hay usuarios registrados. Por favor cree un nuevo usuario." ++ reset
                    putStrLn $ yellow ++ "\n> Presione Enter para continuar..." ++ reset
                    _ <- getLine
                    main
        "2" -> registerNewUser
        "3" -> putStrLn $ magenta ++ bold ++ "\n¡Hasta luego!\n" ++ reset
        _   -> do
                putStrLn $ red ++ "Opción inválida. Por favor seleccione 1, 2 o 3." ++ reset
                putStrLn $ yellow ++ "\n> Presione Enter para continuar..." ++ reset
                _ <- getLine
                main