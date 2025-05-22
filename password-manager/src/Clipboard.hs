module Clipboard (copyToClipboard) where

import System.Process (callCommand)
import System.Info (os)

copyToClipboard :: String -> IO ()
copyToClipboard text = do
    let cmd = case os of
                "mingw32" -> "echo " ++ text ++ " | clip"
                "linux"   -> "echo " ++ text ++ " | xclip -selection clipboard"
                "darwin"  -> "echo " ++ text ++ " | pbcopy"
                _         -> error "Sistema operativo no soportado"
    callCommand cmd
