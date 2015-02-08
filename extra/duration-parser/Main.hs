module Main where

import System.Exit
import System.Environment
import Numeric

main :: IO ()
main =
  do args <- getArgs
     case args of
       []  -> putStr "0"
       [x] -> putStr . show . sum . parse $ x
       _   -> error $ "Got " ++ show args ++ " but don't know how to parse it"

data Time = S | M | H | D

fromChar     :: Char -> Time
fromChar 's' = S
fromChar 'm' = M
fromChar 'h' = H
fromChar 'd' = D
fromChar _   = S

fromTime   :: Num a => Time -> a
fromTime S = 1
fromTime M = 60
fromTime H = 3600
fromTime D = 86400

multiplier :: Num a => Char -> a
multiplier  = fromTime . fromChar

parse    :: String -> [Double]
parse "" = []
parse s  = case readFloat s of
             [(n, "")]     -> [n]
             [(n, (t:s'))] -> n * multiplier t : parse s'
             x             -> error $
                              "Failed to parse '" ++ s ++ "' with result '" ++ show x ++ "'"
