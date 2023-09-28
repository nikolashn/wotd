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

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as B

countLines :: String -> IO Int
countLines path = openFile path ReadMode >>= B.hGetContents
                    >>= return . B.count 10

daysSinceEpoch :: IO Int
daysSinceEpoch = do
  zone <- getCurrentTimeZone
  time <- getCurrentTime
  return $ div (floor (utcTimeToPOSIXSeconds time) + 60 * timeZoneMinutes zone)
               (60 * 60 * 24)

getLineAtIndex :: String -> Int -> IO B.ByteString
getLineAtIndex path i = openFile path ReadMode >>= loop i
  where loop n h | n == 0    = do l <- B.hGetLine h; hClose h; return l
                 | otherwise = B.hGetLine h >> loop (n-1) h

findWordIn :: String -> String -> IO B.ByteString
findWordIn path s = openFile path ReadMode >>= fw
  where fw h = do
                 line <- B.hGetLine h
                 let (_, word) = fromParse $ parseStr $ B.toString line
                 if word == s then hClose h >> return line
                              else fw h

type Parser = Maybe (String, String)

fromParse = fromMaybe $ error "Invalid syntax in dictionary"

runParser :: String -> Parser
runParser s = do
  (rest1, word) <- parseStr s
  (rest2, _) <- parseDropCh ':' rest1
  (_, def) <- parseStr rest2
  return (word, def)

parseStr :: String -> Parser
parseStr s = do
  (rest1, _) <- parseDropCh '"' s
  (rest2, body) <- parseBody rest1
  (rest3, _) <- parseDropCh '"' rest2
  return (rest3, body)

parseDropCh :: Char -> String -> Parser
parseDropCh c ""     = mzero
parseDropCh c (x:xs) = if x == c then return (xs, "") else mzero

parseBodyPlus :: String -> Char -> Parser
parseBodyPlus xs x = do (rs, os) <- parseBody xs; return (rs, x:os)

parseBody :: String -> Parser
parseBody ""     = mzero
parseBody (x:xs) | x == '"'  = return (x:xs, "")
                 | x == '\\' = parseEsc xs
                 | otherwise = parseBodyPlus xs x

parseEsc :: String -> Parser
parseEsc ""     = mzero
parseEsc (x:xs) | x `elem` ['"', '\\', '/'] = parseBodyPlus xs x
                | x == 'n'                  = parseBodyPlus xs '\n'
                | otherwise                 = mzero

bigLine :: String
bigLine = "----------------------------------"

printWord :: B.ByteString -> IO ()
printWord raw = B.putStr $ B.fromString $ unlines $ 
  [ bigLine, "---- " ++ map toUpper word ++ " ----", def, bigLine ]
  where (word, def) = fromParse $ runParser $ B.toString raw

printWOTD :: B.ByteString -> IO ()
printWOTD raw = do
  putStrLn bigLine
  putStrLn "Word of the Day: "
  getZonedTime >>= putStrLn . formatTime defaultTimeLocale "%e %B, %Y"
  printWord raw

main :: IO ()
main = do
  args <- getArgs

  if not $ all (`elem` ["-h", "-r"]) (filter (isPrefixOf "-") args) then
    error "Unexpected option(s). Use wotd -h for a list of options."

  else if ("-h" `elem` args) then putStr $ unlines
    [ "Usage:"
    , "  wotd          Define the word of the day today."
    , "  wotd <WORD>   Define a particular word."
    , "  wotd -h       Display this help menu."
    , "  wotd -r       Define a random word."
    ]

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
      (getLineAtIndex path $ mod d n)
        >>= printWOTD

    else
      (findWordIn path $ map toLower $ intercalate " " args)
        >>= printWord

