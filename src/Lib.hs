module Lib where

import           Data.Char  (isLetter, isUpper, toLower, toUpper)
import           Data.List  (elemIndex, elemIndices, find, nub, sortBy)
import           Data.Maybe (fromMaybe)
import           Data.Tuple (swap)

phone :: [(Char, String)]
phone =
  [ ('0', " +_0")
  , ('1', "1")
  , ('2', "ABC2")
  , ('3', "DEF3")
  , ('4', "GHI4")
  , ('5', "JKL5")
  , ('6', "MNO6")
  , ('7', "PQRS7")
  , ('8', "TUV8")
  , ('9', "WXYZ9")
  , ('*', "^*")
  , ('#', ".,#")
  ]

convo :: [String]
convo =
  [ "Wanna play 20 questions"
  , "Ya"
  , "U 1st haha"
  , "Lol ok. Have u ever tasted alcohol"
  , "Lol ya"
  , "Wow ur cool haha. Ur turn"
  , "Ok. Do u think I am pretty Lol"
  , "Lol ya"
  , "Just making sure rofl ur turn"
  ]

-- validButtons = "1234567890*#"
type Digit = Char

-- Valid presses: 1 and up
type Presses = Int

-- | reverseTaps
-- >>> reverseTaps 'F' 
-- [('*',1),('3',3)]
-- >>> reverseTaps '*' 
-- [('*',2)]
reverseTaps :: Char -> [(Digit, Presses)]
reverseTaps d =
  if isUpper d
    then ('*', 1) : low
    else low
  where
    upperD = toUpper d
    found = find (elem upperD . snd) phone
    cell =
      fromMaybe
        (error ("symbol " ++ [d] ++ " was not found in the phone"))
        found
    (Just chrIndex) = elemIndex (toUpper d) (snd cell)
    low = [(fst cell, succ chrIndex)]

-- assuming the default phone definition
-- 'a' -> [('2', 1)]
-- 'A' -> [('*', 1), ('2', 1)]
fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps dp = sum (map snd dp)

cellPhonesDead :: String -> [(Digit, Presses)]
cellPhonesDead = concatMap reverseTaps

symbolCost :: Char -> Presses
symbolCost = fingerTaps . reverseTaps

symbolCostInSentence :: String -> Char -> (Char, Presses)
symbolCostInSentence sent c = (loC, totalCost)
  where
    loC = toLower c
    upC = toUpper c
    cost l = symbolCost l * length (elemIndices l sent)
    totalCost = cost upC + cost loC

-- | mostPopularLetter
-- >>> mostPopularLetter "foo bar baz" 
-- ('o',6)
mostPopularLetter :: String -> (Char, Presses)
mostPopularLetter msg =
  swap $ maximum $ nub $ map (swap . symbolCostInSentence msg) msg

coolestLtr :: [String] -> (Char, Presses)
coolestLtr messages = mostPopularLetter (concat messages)

wordCost' :: String -> Presses
wordCost' s = sum (map (snd . symbolCostInSentence s) s)

wordCost :: String -> Presses
wordCost = sum . map symbolCost

toLowerStr :: String -> String
toLowerStr = map toLower

toLowerStrOnlyLetters :: String -> String
toLowerStrOnlyLetters = toLowerStr . filter isLetter

filterNonLetters :: [String] -> [String]
filterNonLetters = map (filter isLetter)

-- coolWords transforms list of messages into the list of words and calculates cost for every word,
-- stores them as tuple into [(word,Presses)] list. Then transform the list [(word0, 10),(word1,11) , (word0, 10) ..]
-- with repetive items to the list with calculated all Presses for the same word .
-- Last step is sorting by Presses number.
-- Function also takes into account case when word have different censitivity in  a sentence.
coolWords :: [String] -> [(String, Presses)]
coolWords messages =
  sortBy (flip (compare . snd) . snd) (nub $ map (mrg arr . fst) arr)
  where
    mrg y x = (x, foldr ((+) . snd) 0 (filter ((== x) . fst) y))
    wordList = filterNonLetters (concatMap words messages)
    arr = zipWith (\x y -> (toLowerStr x, y)) wordList (map wordCost wordList)

coolestWord :: [String] -> String
coolestWord []       = ""
coolestWord messages = fst (head (coolWords messages))

sortBtnUsagesInText :: [String] -> [(Digit, Presses)]
sortBtnUsagesInText messages =
  sortBy (\a b -> compare (snd b) (snd a)) (nub $ map (mrg lst . fst) lst)
  where
    mrg y x = (x, foldr ((+) . snd) 0 (filter ((== x) . fst) y))
    lst = cellPhonesDead (concat messages)


