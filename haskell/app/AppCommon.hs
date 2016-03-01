module AppCommon
       ( readFile'
       , maybeTest, testPrint
       ) where

import Text.Printf

show2d :: Int -> String
show2d = printf "%2d"

show02d :: Int -> String
show02d = printf "%02d"

readFile' :: Int -> Int -> String -> IO String
readFile' n m fileName = readFile $ "../set" ++ show02d n ++ "/" ++ show02d m ++ "/" ++ fileName

greenStr :: String
greenStr = "\ESC[92m"

redStr :: String
redStr = "\ESC[91m"

resetStr :: String
resetStr = "\ESC[0m"

toGreen :: String -> String
toGreen s = greenStr ++ s ++ resetStr

toRed :: String -> String
toRed s = redStr ++ s ++ resetStr

challengeString :: Int -> Int -> String
challengeString n m = "set " ++ show n ++ ", challenge " ++ show m

testPrint :: Int -> Int -> Bool -> IO ()
testPrint n m cond = putStrLn $ challengeString n m ++ ":  " ++ statusString
  where statusString
          | cond      = toGreen "OK"
          | otherwise = toRed   "error"

maybeTest :: (a -> Bool) -> Maybe a -> Bool
maybeTest f m = case m of
  Just j  -> f j
  Nothing -> False
