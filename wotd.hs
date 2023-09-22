module Main where

import Control.Monad
import Data.Char
import Data.Maybe
import Data.Time
import Data.Time.Clock.POSIX
import System.Environment
import System.IO

countLines :: String -> IO Int
countLines path =
  do h <- openFile path ReadMode; n <- cl h; hClose h; return n
  where cl h = do
                 eof <- hIsEOF h
                 if eof then do return 0
                        else do hGetLine h; n <- cl h; return (1 + n)

daysSinceEpoch :: IO Int
daysSinceEpoch = do
  zone <- getCurrentTimeZone
  time <- getCurrentTime
  return $ (floor (utcTimeToPOSIXSeconds time) + 60 * timeZoneMinutes zone)
           `div` (60 * 60 * 24)

getLineAtIndex :: String -> Int -> IO String
getLineAtIndex path i =
  do h <- openFile path ReadMode; s <- gl h i; hClose h; return s
  where gl h n = do
                   eof <- hIsEOF h
                   if eof then
                     return $ error $ "Line " ++ show i ++ " of "
                                      ++ path ++ " not read."
                   else
                     do
                       s <- hGetLine h
                       if n == 0 then return s else gl h (n-1)

type Parser = Maybe (String, String)

runParser :: String -> (String, String)
runParser s = (word, def)
  where (rest1, word) = fromJust $ parseStr s
        (rest2, _)    = fromJust $ parseDropCh ':' rest1
        (rest3, def)  = fromJust $ parseStr rest2

parseStr :: String -> Parser
parseStr s = do
  (rest1, _) <- parseDropCh '"' s
  (rest2, body) <- parseBody rest1
  (rest3, _) <- parseDropCh '"' rest2
  return (rest3, body)

parseDropCh :: Char -> String -> Parser
parseDropCh c ""     = mzero
parseDropCh c (x:xs) = do if x == c then return (xs, "") else mzero

parseBodyPlus :: String -> Char -> Parser
parseBodyPlus xs x = do (rs, os) <- parseBody xs; return (rs, x:os)

parseBody :: String -> Parser
parseBody ""     = mzero
parseBody (x:xs) | x == '"'  = return (x:xs, "")
                 | x == '\\' = parseEsc xs
                 | otherwise = parseBodyPlus xs x

parseEsc :: String -> Parser
parseEsc ""       = mzero
parseEsc (x:xs)   | x `elem` ['"', '\\', '/'] = parseBodyPlus xs x
                  | x == 'n'                  = parseBodyPlus xs '\n'
                  | otherwise                 = mzero

main :: IO ()
main = do
  home <- getEnv "HOME"
  let path = home ++ "/.config/wotd/dictionary.txt"

  d <- daysSinceEpoch
  l <- countLines path
  wotdRaw <- getLineAtIndex path (d `mod` l)

  zt <- getZonedTime
  putStrLn "----------------------------------"
  putStr "Word of the day: "
  putStrLn $ formatTime defaultTimeLocale "%e %B, %Y" zt

  let (word, def) = runParser wotdRaw
  putStrLn $ "---- " ++ map toUpper word ++ " ----"
  putStrLn def
  putStrLn "----------------------------------"

