module Main where

import           Phone

main :: IO ()
main = do
  putStrLn ("Coolest word " ++ show (coolestWord convo))
  putStrLn ("Button popularity " ++ show (sortBtnUsagesInText convo))
