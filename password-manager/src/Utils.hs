module Utils (padRight, maskString, clearScreen) where

import qualified System.Console.ANSI as ANSI

padRight :: Int -> String -> String
padRight n s = s ++ replicate (n - length s) ' '

maskString :: String -> String
maskString s = if length s <= 3 then replicate (length s) '*'
               else take 2 s ++ replicate (length s - 2) '*'

clearScreen :: IO ()
clearScreen = do
    ANSI.clearScreen
    ANSI.setCursorPosition 0 0