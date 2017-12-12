module Main where

import Lib

main :: IO ()
main = do
  putStrLn ("Coolest word " ++ show (coolestWord convo))
  putStrLn ("Button popularity " ++ show (sortBtnUsagesInText convo))
