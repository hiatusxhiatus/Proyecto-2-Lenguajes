module UI (registerNewUser, loginUser, mainMenu) where

import Types
import Encryption
import Storage (saveUserData, loadUserData, getHiddenInput, getVisibleInput, userExists)
import Clipboard
import Utils

import System.IO (hSetEcho, stdin, hFlush, stdout)
import Data.Char (isDigit, toLower, isAlphaNum)
import Data.List (find, deleteBy, isInfixOf)
import Control.Monad (when)

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

-- Función auxiliar para pausar y esperar input del usuario
waitForEnter :: IO ()
waitForEnter = do
    putStr $ yellow ++ "\n> Presione Enter para continuar..." ++ reset
    hFlush stdout
    _ <- getLine
    return ()

-- Función auxiliar para mostrar prompt de entrada
showInputPrompt :: String -> IO ()
showInputPrompt msg = do
    putStr $ cyan ++ "\n> " ++ msg ++ ": " ++ reset
    hFlush stdout

-- Función auxiliar para mostrar prompt de entrada oculta
showHiddenInputPrompt :: String -> IO ()
showHiddenInputPrompt msg = do
    putStr $ cyan ++ "\n> " ++ msg ++ " (entrada oculta): " ++ reset
    hFlush stdout

-- Validar username
isValidUsername :: String -> Bool
isValidUsername username = 
    length username >= 3 && 
    length username <= 20 && 
    all (\c -> isAlphaNum c || c == '_') username

-- Registro de nuevo usuario
registerNewUser :: IO ()
registerNewUser = do
    clearScreen
    putStrLn $ cyan ++ bold ++ replicate 60 '='
    putStrLn "       ✧/ᐠ-ꞈ-ᐟ\\    REGISTRO DE NUEVO USUARIO   /ᐠ-ꞈ-ᐟ\\✧"
    putStrLn $ replicate 60 '=' ++ reset
    putStrLn "\nCreando un nuevo usuario en el sistema..."
    putStrLn $ yellow ++ "Requisitos:" ++ reset
    putStrLn "   • Usuario: 3-20 caracteres (letras, números, guión bajo)"
    putStrLn "   • PIN: mínimo 4 dígitos numéricos"
    
    showInputPrompt "Ingrese su nombre de usuario"
    username <- getVisibleInput
    
    if not (isValidUsername username)
        then do
            putStrLn $ red ++ "ERROR: Username inválido." ++ reset
            putStrLn "   Debe tener 3-20 caracteres (solo letras, números y guión bajo)."
            waitForEnter
            registerNewUser
        else do
            exists <- userExists username
            if exists
                then do
                    putStrLn $ red ++ "ERROR: El usuario '" ++ username ++ "' ya existe." ++ reset
                    putStrLn "   Por favor elija un nombre diferente."
                    waitForEnter
                    registerNewUser
                else do
                    showHiddenInputPrompt "Ingrese un PIN seguro (mínimo 4 dígitos)"
                    pin <- getHiddenInput
                    putStrLn ""
                    
                    if length pin < 4 || not (all isDigit pin)
                        then do
                            putStrLn $ red ++ "ERROR: PIN inválido. Debe tener al menos 4 dígitos numéricos." ++ reset
                            waitForEnter
                            registerNewUser
                        else do
                            showHiddenInputPrompt "Confirme su PIN"
                            confirmPin <- getHiddenInput
                            putStrLn ""
                            
                            if pin == confirmPin
                                then do
                                    let newUser = User username (encryptPassword pin pin) []
                                    saveUserData newUser
                                    putStrLn $ green ++ "✓ ¡Usuario creado con éxito!" ++ reset
                                    putStrLn $ "   Usuario: " ++ cyan ++ username ++ reset
                                    putStrLn "   Ya puede comenzar a administrar sus contraseñas."
                                    waitForEnter
                                    mainMenu newUser
                                else do
                                    putStrLn $ red ++ "ERROR: Los PINs no coinciden." ++ reset
                                    waitForEnter
                                    registerNewUser

-- Login de usuario existente
loginUser :: IO ()
loginUser = loginAttempt 0
  where
    loginAttempt :: Int -> IO ()
    loginAttempt attempts
        | attempts >= 3 = do
            putStrLn $ red ++ "Demasiados intentos fallidos. Saliendo por seguridad..." ++ reset
            return ()
        | otherwise = do
            clearScreen
            putStrLn $ cyan ++ bold ++ replicate 60 '='
            putStrLn "         =＾• ⋏ •＾=  INICIAR SESIÓN  =＾• ⋏ •＾="
            putStrLn $ replicate 60 '=' ++ reset
            putStrLn "\nIngrese sus credenciales para acceder al sistema..."
            when (attempts > 0) $ 
                putStrLn $ yellow ++ "⚠️  Intento " ++ show (attempts + 1) ++ " de 3" ++ reset
            
            showInputPrompt "Nombre de usuario"
            username <- getVisibleInput
            
            showHiddenInputPrompt "PIN"
            inputPin <- getHiddenInput
            putStrLn ""
            
            maybeUser <- loadUserData username inputPin
            case maybeUser of
                Just user -> do
                    putStrLn $ green ++ "✓ Acceso concedido!" ++ reset
                    putStrLn $ "   Bienvenido de vuelta, " ++ cyan ++ username ++ reset ++ "!"
                    putStrLn $ "   Tienes " ++ show (length (passwords user)) ++ " contraseñas guardadas."
                    waitForEnter
                    mainMenu user
                Nothing -> do
                    putStrLn $ red ++ "Usuario o PIN incorrecto." ++ reset
                    if attempts == 0
                        then do
                            putStrLn "\n💡 ¿Es la primera vez que usa el sistema?"
                            showInputPrompt "¿Desea registrar un nuevo usuario? (s/n)"
                            response <- getLine
                            if map toLower response == "s"
                                then registerNewUser
                                else loginAttempt (attempts + 1)
                        else loginAttempt (attempts + 1)

-- Menú principal
mainMenu :: User -> IO ()
mainMenu user = do
    clearScreen
    putStrLn $ cyan ++ bold ++ replicate 60 '='
    putStrLn "       /ᐠ.ᴗ.ᐟ\\   ADMINISTRADOR DE CONTRASEÑAS /ᐠ｡‸｡ᐟ\\"
    putStrLn $ replicate 60 '=' ++ reset
    putStrLn $ "\n👤 Usuario: " ++ cyan ++ username user ++ reset
    putStrLn $ "📊 Estado: " ++ show (length (passwords user)) ++ " contraseñas guardadas"
    putStrLn $ yellow ++ "\nOPCIONES DISPONIBLES:" ++ reset
    putStrLn "   1. 📋 Ver todas las contraseñas"
    putStrLn "   2. ➕ Agregar nueva contraseña"
    putStrLn "   3. 🔍 Buscar contraseña"
    putStrLn "   4. ✏️  Modificar contraseña"
    putStrLn "   5. 🗑️  Eliminar contraseña"
    putStrLn "   6. 📋 Copiar al portapapeles"
    putStrLn "   7. 🔐 Cambiar PIN"
    putStrLn "   8. 🔄 Cambiar de usuario"
    putStrLn "   9. 🚪 Salir y guardar"
    
    showInputPrompt "Seleccione una opción (1-9)"
    choice <- getLine
    
    case choice of
        "1" -> viewAllPasswords user >>= mainMenu
        "2" -> addNewPassword user >>= mainMenu
        "3" -> searchPassword user >> mainMenu user
        "4" -> modifyPassword user >>= mainMenu
        "5" -> deletePassword user >>= mainMenu
        "6" -> copyToClipboardMenu user >> mainMenu user
        "7" -> changePIN user >>= mainMenu
        "8" -> do
            saveUserData user
            putStrLn $ green ++ "\n✓ Datos guardados. Cambiando de usuario..." ++ reset
            waitForEnter
            loginUser
        "9" -> do
            saveUserData user
            putStrLn $ green ++ "\n✓ Datos guardados correctamente." ++ reset
            putStrLn $ "¡Hasta luego, " ++ cyan ++ username user ++ reset
        _   -> do
            putStrLn $ red ++ "Opción inválida. Por favor seleccione un número del 1 al 9." ++ reset
            waitForEnter
            mainMenu user

-- Mostrar todas las contraseñas
viewAllPasswords :: User -> IO User
viewAllPasswords user = do
    clearScreen
    putStrLn $ cyan ++ bold ++ replicate 60 '='
    putStrLn "              CONTRASEÑAS GUARDADAS (= ʘ ᆽ   ʘ =)"
    putStrLn $ replicate 60 '=' ++ reset
    putStrLn $ "\n👤 Usuario: " ++ cyan ++ username user ++ reset
    
    if null (passwords user)
        then do
            putStrLn "\n📝 No tienes contraseñas guardadas aún."
            putStrLn "   Usa la opción 2 del menú principal para agregar tu primera contraseña."
        else do
            putStrLn ""
            mapM_ (\entry -> do
                putStrLn $ bold ++ "Título: " ++ reset ++ title entry
                putStrLn $ bold ++ "Usuario: " ++ reset ++ maskString (Types.serviceName entry)
                putStrLn $ bold ++ "Contraseña: " ++ reset ++ "**********"
                putStrLn $ replicate 40 '-'  -- separador simple
                ) (passwords user)
            
            putStrLn $ "\n📊 Total: " ++ show (length (passwords user)) ++ " contraseñas"
    
    waitForEnter
    return user

-- Agregar nueva contraseña
addNewPassword :: User -> IO User
addNewPassword user = do
    clearScreen
    putStrLn $ cyan ++ bold ++ replicate 60 '='
    putStrLn "     =＾● ⋏ ●＾= AGREGAR NUEVA CONTRASEÑA =＾● ⋏ ●＾="
    putStrLn $ replicate 60 '=' ++ reset
    putStrLn $ "\n👤 Usuario: " ++ cyan ++ username user ++ reset
    putStrLn "📝 Complete la información de la nueva contraseña (escriba 'cancelar' para salir):\n"

    -- Título
    showInputPrompt "Título/Descripción (ej: Gmail, Facebook, etc.)"
    t <- getVisibleInput
    if map toLower t == "cancelar"
        then do
            putStrLn $ red ++ "\nOperación cancelada por el usuario." ++ reset
            waitForEnter
            return user
        else if null t
            then do
                putStrLn $ red ++ "❌ Este campo no puede estar vacío." ++ reset
                waitForEnter
                addNewPassword user
            else do  -- <== Cambiar esto a do para empezar bloque anidado
                -- Usuario/email
                showInputPrompt "Nombre de usuario/email para esta cuenta"
                u <- getVisibleInput
                if map toLower u == "cancelar"
                    then do
                        putStrLn $ red ++ "\nOperación cancelada por el usuario." ++ reset
                        waitForEnter
                        return user
                    else if null u
                        then do
                            putStrLn $ red ++ "❌ Este campo no puede estar vacío." ++ reset
                            waitForEnter
                            addNewPassword user
                        else do
                            -- Contraseña
                            showHiddenInputPrompt "Contraseña"
                            p <- getHiddenInput
                            putStrLn ""
                            if null p
                                then do
                                    putStrLn $ red ++ "❌ La contraseña no puede estar vacía." ++ reset
                                    waitForEnter
                                    addNewPassword user
                                else do
                                    -- Confirmación
                                    showHiddenInputPrompt "Confirme la contraseña"
                                    confirmP <- getHiddenInput
                                    putStrLn ""
                                    if null confirmP
                                        then do
                                            putStrLn $ red ++ "❌ Debe confirmar la contraseña." ++ reset
                                            waitForEnter
                                            addNewPassword user
                                        else if p /= confirmP
                                            then do
                                                putStrLn $ red ++ "❌ Las contraseñas no coinciden." ++ reset
                                                waitForEnter
                                                addNewPassword user
                                            else do
                                                -- Aquí va la lógica para guardar la contraseña
                                                case find (\e -> map toLower (title e) == map toLower t) (passwords user) of
                                                    Just _ -> do
                                                        putStrLn $ yellow ++ "⚠️  Ya existe una contraseña con ese título." ++ reset
                                                        showInputPrompt "¿Desea reemplazarla? (s/n)"
                                                        response <- getLine
                                                        if map toLower response == "s"
                                                            then do
                                                                let filtered = filter (\e -> map toLower (title e) /= map toLower t) (passwords user)
                                                                    encryptedPass = encryptPassword (pin user) p
                                                                    newEntry = PasswordEntry t u encryptedPass
                                                                    updated = user { passwords = newEntry : filtered }
                                                                saveUserData updated
                                                                putStrLn $ green ++ "✓ Contraseña reemplazada con éxito!" ++ reset
                                                                waitForEnter
                                                                return updated
                                                            else do
                                                                putStrLn $ red ++ "Operación cancelada." ++ reset
                                                                waitForEnter
                                                                return user
                                                    Nothing -> do
                                                        let encryptedPass = encryptPassword (pin user) p
                                                            newEntry = PasswordEntry t u encryptedPass
                                                            updated = user { passwords = newEntry : passwords user }
                                                        saveUserData updated
                                                        putStrLn $ green ++ "✓ ¡Contraseña agregada con éxito!" ++ reset
                                                        putStrLn $ "   Total de contraseñas: " ++ show (length (passwords updated))
                                                        waitForEnter
                                                        return updated


-- Funciones auxiliares
promptNonEmpty :: String -> IO String
promptNonEmpty prompt = do
    showInputPrompt prompt
    input <- getVisibleInput
    if null input
        then do
            putStrLn $ red ++ "⚠️  Este campo no puede estar vacío." ++ reset
            promptNonEmpty prompt
        else return input

promptPassword :: String -> IO String
promptPassword prompt = do
    showHiddenInputPrompt prompt
    getHiddenInput

cancelAdd :: IO a
cancelAdd = do
    putStrLn $ red ++ "\nOperación cancelada por el usuario." ++ reset
    waitForEnter
    fail "Operación cancelada"

-- Buscar contraseña
searchPassword :: User -> IO ()
searchPassword user = do
    clearScreen
    putStrLn $ cyan ++ bold ++ replicate 60 '='
    putStrLn "             (^･ｪ･^)  BUSCAR CONTRASEÑA  (^･ｪ･^)"
    putStrLn $ replicate 60 '=' ++ reset
    putStrLn $ "\n👤 Usuario: " ++ cyan ++ username user ++ reset
    
    if null (passwords user)
        then do
            putStrLn "\n📝 No tienes contraseñas guardadas para buscar."
        else do
            showInputPrompt "Buscar por título (búsqueda parcial)"
            term <- getLine
            let results = filter (\e -> map toLower term `isInfixOf` map toLower (title e)) (passwords user)
            
            if null results
                then putStrLn $ red ++ "No se encontraron contraseñas que contengan: \"" ++ term ++ "\"" ++ reset
                else do
                    putStrLn $ green ++ "\n✓ Se encontraron " ++ show (length results) ++ " coincidencia(s):\n" ++ reset
                    mapM_ (\(i, e) -> do
                        putStrLn $ "🔹 " ++ show i ++ ". " ++ title e
                        putStrLn $ "   Usuario: " ++ Types.serviceName e
                        putStrLn $ "   Contraseña: ********"
                        putStrLn "") (zip [1..] results)
    
    waitForEnter
    return ()

-- Modificar contraseña
modifyPassword :: User -> IO User
modifyPassword user = do
    clearScreen
    putStrLn $ cyan ++ bold ++ replicate 60 '='
    putStrLn "              ᓚᘏᗢ   MODIFICAR CONTRASEÑA   ᓚᘏᗢ"
    putStrLn $ replicate 60 '=' ++ reset
    putStrLn $ "\n👤 Usuario: " ++ cyan ++ username user ++ reset

    if null (passwords user)
        then do
            putStrLn "\n📝 No tienes contraseñas guardadas para modificar."
            waitForEnter
            return user
        else do
            showInputPrompt "Título exacto a modificar"
            t <- getLine
            let ps = passwords user
            case find (\e -> title e == t) ps of
                Nothing -> do
                    putStrLn $ red ++ "No se encontró ninguna contraseña con el título: \"" ++ t ++ "\"" ++ reset
                    putStrLn "\n💡 Consejo: Use la opción de búsqueda para ver los títulos exactos."
                    waitForEnter
                    return user
                Just entry -> do
                    putStrLn $ green ++ "\n✓ Contraseña encontrada: " ++ title entry ++ reset
                    putStrLn $ "   Usuario/email actual: " ++ Types.serviceName entry
                    putStrLn "\n📝 Ingrese los nuevos datos (Enter para mantener el actual):"

                    showInputPrompt $ "Nuevo usuario/email [actual: " ++ Types.serviceName entry ++ "]"
                    newService <- getLine
                    let finalService = if null newService then Types.serviceName entry else newService

                    showHiddenInputPrompt "Nueva contraseña [Enter para mantener actual]"
                    newPass <- getHiddenInput
                    putStrLn ""

                    if null newPass
                        then do
                            -- Solo cambia el usuario/email
                            let updatedEntry = entry { Types.serviceName = finalService }
                                updatedList = updatedEntry : filter (\e -> title e /= t) ps
                                newUserData = user { passwords = updatedList }
                            saveUserData newUserData
                            putStrLn $ green ++ "✓ Usuario actualizado con éxito!" ++ reset
                            waitForEnter
                            return newUserData
                        else do
                            showHiddenInputPrompt "Confirme la nueva contraseña"
                            confirmPass <- getHiddenInput
                            putStrLn ""
                            if newPass == confirmPass
                                then do
                                    -- Cambia usuario/email y contraseña
                                    let encryptedPass = encryptPassword (pin user) newPass
                                        updatedEntry = entry { Types.serviceName = finalService, encryptedPassword = encryptedPass }
                                        updatedList = updatedEntry : filter (\e -> title e /= t) ps
                                        newUserData = user { passwords = updatedList }
                                    saveUserData newUserData
                                    putStrLn $ green ++ "✓ Usuario y contraseña actualizados con éxito!" ++ reset
                                    waitForEnter
                                    return newUserData
                                else do
                                    putStrLn $ red ++ "ERROR: Las contraseñas no coinciden." ++ reset
                                    waitForEnter
                                    return user


-- Eliminar contraseña
deletePassword :: User -> IO User
deletePassword user = do
    clearScreen
    putStrLn $ cyan ++ bold ++ replicate 60 '='
    putStrLn "                ✖✖✖  ELIMINAR CONTRASEÑA  ✖✖✖"
    putStrLn $ replicate 60 '=' ++ reset
    putStrLn $ "\n👤 Usuario: " ++ cyan ++ username user ++ reset
    
    if null (passwords user)
        then do
            putStrLn "\n📝 No tienes contraseñas guardadas para eliminar."
            waitForEnter
            return user
        else do
            showInputPrompt "Título exacto a eliminar"
            t <- getLine
            let ps = passwords user
                filtered = filter (\e -> title e /= t) ps
            
            if length ps == length filtered
                then do
                    putStrLn $ red ++ "No se encontró ninguna contraseña con el título: \"" ++ t ++ "\"" ++ reset
                    waitForEnter
                    return user
                else do
                    putStrLn $ yellow ++ "⚠️  ¿Está seguro de eliminar la contraseña: \"" ++ t ++ "\"?" ++ reset
                    putStrLn "   Esta acción no se puede deshacer."
                    showInputPrompt "Confirmar eliminación (escriba 'ELIMINAR' para confirmar)"
                    confirmation <- getLine
                    
                    if confirmation == "ELIMINAR"
                        then do
                            let newUserData = user { passwords = filtered }
                            saveUserData newUserData
                            putStrLn $ green ++ "✓ Contraseña eliminada con éxito." ++ reset
                            putStrLn $ "   Contraseñas restantes: " ++ show (length filtered)
                            waitForEnter
                            return newUserData
                        else do
                            putStrLn $ red ++ "Eliminación cancelada." ++ reset
                            waitForEnter
                            return user

-- Copiar al portapapeles
copyToClipboardMenu :: User -> IO ()
copyToClipboardMenu user = do
    clearScreen
    putStrLn $ cyan ++ bold ++ replicate 60 '='
    putStrLn "     =＾● ㉨ ●＾=   COPIAR AL PORTAPAPELES   =＾● ㉨ ●＾="
    putStrLn $ replicate 60 '=' ++ reset
    putStrLn $ "\n👤 Usuario: " ++ cyan ++ username user ++ reset
    
    if null (passwords user)
        then do
            putStrLn "\n📝 No tienes contraseñas guardadas para copiar."
        else do
            putStrLn "\n📋 Seleccione la contraseña a copiar:\n"
            
            let entries = passwords user
            mapM_ (\(i, entry) -> putStrLn $ "   " ++ show i ++ ". " ++ title entry) (zip [1..] entries)
            
            showInputPrompt $ "Ingrese el número (1-" ++ show (length entries) ++ ") o 0 para cancelar"
            choice <- getLine
            
            case reads choice of
                [(n, "")] | n > 0 && n <= length entries -> do
                    let entry = entries !! (n-1)
                    putStrLn $ green ++ "\n✓ Seleccionado: " ++ title entry ++ reset
                    putStrLn "\n¿Qué desea copiar?"
                    putStrLn "   1. Copiar usuario"
                    putStrLn "   2. Copiar contraseña"
                    
                    showInputPrompt "Seleccione (1-2)"
                    subChoice <- getLine
                    
                    case subChoice of
                        "1" -> do
                            copyToClipboard (Types.serviceName entry)
                            putStrLn $ green ++ "✓ ¡Usuario copiado al portapapeles!" ++ reset
                            putStrLn "   Ya puede pegarlo donde lo necesite."
                        "2" -> do
                            let decryptedPass = decryptPassword (pin user) (encryptedPassword entry)
                            copyToClipboard decryptedPass
                            putStrLn $ green ++ "✓ ¡Contraseña copiada al portapapeles!" ++ reset
                            putStrLn "   Ya puede pegarla donde la necesite."
                        _   -> putStrLn $ red ++ "Opción inválida" ++ reset
                [(0, "")] -> putStrLn $ red ++ "Operación cancelada" ++ reset
                _ -> putStrLn $ red ++ "Entrada inválida" ++ reset
    
    waitForEnter
    return ()

-- Cambiar PIN
changePIN :: User -> IO User
changePIN user = do
    clearScreen
    putStrLn $ cyan ++ bold ++ replicate 60 '='
    putStrLn "      （Φ ﻌ Φ）   CAMBIAR PIN DE SEGURIDAD   （Φ ﻌ Φ）"
    putStrLn $ replicate 60 '=' ++ reset
    putStrLn $ "\n👤 Usuario: " ++ cyan ++ username user ++ reset
    putStrLn "🔐 Para cambiar su PIN, primero debe confirmar el actual."
    
    showHiddenInputPrompt "Ingrese su PIN actual"
    currentPin <- getHiddenInput
    putStrLn ""
    
    if currentPin /= pin user
        then do
            putStrLn $ red ++ "PIN actual incorrecto." ++ reset
            waitForEnter
            return user
        else do
            showHiddenInputPrompt "Ingrese el nuevo PIN (mínimo 4 dígitos)"
            newPin <- getHiddenInput
            putStrLn ""
            
            if length newPin < 4 || not (all isDigit newPin)
                then do
                    putStrLn $ red ++ "PIN inválido. Debe tener al menos 4 dígitos numéricos." ++ reset
                    waitForEnter
                    changePIN user
                else do
                    showHiddenInputPrompt "Confirme el nuevo PIN"
                    confirmPin <- getHiddenInput
                    putStrLn ""
                    
                    if newPin == confirmPin
                        then do
                            let reencryptedPasswords = map (\entry -> 
                                    let decrypted = decryptPassword (pin user) (encryptedPassword entry)
                                        newEncrypted = encryptPassword newPin decrypted
                                    in entry { encryptedPassword = newEncrypted }) (passwords user)
                            let newUser = user { pin = encryptPassword newPin newPin, passwords = reencryptedPasswords }
                            saveUserData newUser
                            putStrLn $ green ++ "✓ PIN cambiado con éxito!" ++ reset
                            putStrLn "   Todas sus contraseñas han sido re-encriptadas con el nuevo PIN."
                            waitForEnter
                            return newUser
                        else do
                            putStrLn $ red ++ "ERROR: Los PINs no coinciden." ++ reset
                            waitForEnter
                            changePIN user