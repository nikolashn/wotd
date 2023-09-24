module Main where

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.Time
import Data.Time.Clock.POSIX
import System.Environment
import System.IO
import System.Random

countLines :: String -> IO Int
countLines path = openFile path ReadMode >>= cl
  where cl h = do
                 eof <- hIsEOF h
                 if eof then hClose h >> return 0
                        else hGetLine h >> cl h >>= \n -> return (1 + n)

daysSinceEpoch :: IO Int
daysSinceEpoch = do
  zone <- getCurrentTimeZone
  time <- getCurrentTime
  return $ div (floor (utcTimeToPOSIXSeconds time) + 60 * timeZoneMinutes zone)
               (60 * 60 * 24)

getLineAtIndex :: String -> Int -> IO String
getLineAtIndex path i = openFile path ReadMode >>= gl i
  where gl n h = do
                   line <- hGetLine h
                   if n == 0 then hClose h >> return line
                             else gl (n-1) h

findWordIn :: String -> String -> IO (String)
findWordIn path s = openFile path ReadMode >>= fw
  where fw h = do
                 line <- hGetLine h
                 let (word, _) = runParser line
                 if word == s then hClose h >> return line
                              else fw h

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

printWord :: String -> IO ()
printWord raw = do
  let (word, def) = runParser raw
  putStrLn "----------------------------------"
  putStrLn $ "---- " ++ map toUpper word ++ " ----"
  putStrLn def
  putStrLn "----------------------------------"

printWOTD :: String -> IO ()
printWOTD raw = do
  putStrLn "----------------------------------"
  putStr "Word of the Day: "
  getZonedTime >>= putStrLn . formatTime defaultTimeLocale "%e %B, %Y"
  printWord raw

main :: IO ()
main = do
  args <- getArgs

  if not $ all (`elem` ["-h", "-r"]) (filter (isPrefixOf "-") args) then
    error "Unexpected options. Use wotd -h for a list of options."

  else if ("-h" `elem` args) then do
    putStrLn "Usage:"
    putStrLn "  wotd          Define the word of the day today."
    putStrLn "  wotd <WORD>   Define a particular word."
    putStrLn "  wotd -h       Display this help menu."
    putStrLn "  wotd -r       Define a random word."

  else do -- Options that print a word
    home <- getEnv "HOME"
    let path = home ++ "/.config/wotd/dictionary.txt"

    if ("-r" `elem` args) then do
      n <- countLines path
      r <- randomIO
      (getLineAtIndex path $ fst $ randomR (0, n - 1) $ mkStdGen r)
        >>= printWord

    else if null args then do
      n <- countLines path
      d <- daysSinceEpoch
      getLineAtIndex path (d `mod` n) >>= printWOTD

    else findWordIn path (intercalate " " args) >>= printWord

