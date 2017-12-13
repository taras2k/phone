module Main where

import           Data.Char       (isLetter, isLower)
import           Data.List       (all, findIndices, maximumBy)
import           Phone
import           Test.QuickCheck

allowedChars :: String
allowedChars =
  ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ ['^', '_', '*', '.', ',']

charGen :: Gen Char
charGen = elements allowedChars

wordGen :: Gen String
wordGen = listOf charGen

textGen :: Gen [String]
textGen = listOf wordGen

newtype PhoneWord =
  PhoneWord String
  deriving (Eq, Show)

phoneWordGen :: Gen PhoneWord
phoneWordGen = do
  word <- wordGen
  return (PhoneWord word)

instance Arbitrary PhoneWord where
  arbitrary = phoneWordGen

newtype PhoneText =
  PhoneText [String]
  deriving (Eq, Show)

phoneTextGen :: Gen PhoneText
phoneTextGen = do
  text <- textGen
  return (PhoneText text)

instance Arbitrary PhoneText where
  arbitrary = phoneTextGen

-- checks that coolest contains only letters
prop_coolestWord :: PhoneText -> Bool
prop_coolestWord (PhoneText txt) = all isLetter (coolestWord txt)

-- check if returned list is sorted
prop_coolWords :: Property
prop_coolWords =
  forAll
    textGen
    (\text ->
       let coolText = coolWords text
           cwMax = snd (maximumBy (\x y -> (compare (snd x) (snd y))) coolText)
       in (null coolText || (cwMax == snd (head coolText))))

-- check if returned list contains unique words
prop_coolWordsUnique :: Property
prop_coolWordsUnique =
  forAll
    textGen
    (\text ->
       let coolText = coolWords text
           cw = fst (head coolText)
           len = (length . findIndices ((cw ==) . fst) ) coolText
       in ( null coolText || 1 == len ) )

-- check toLowerStrOnlyLetters
prop_toLowerStrOnlyLetters :: PhoneWord -> Bool
prop_toLowerStrOnlyLetters (PhoneWord word) =
  let wrd = toLowerStrOnlyLetters word
  in all (\c -> isLower c && isLetter c) wrd

main :: IO ()
main = do
  putStrLn "Test coolestWord :  check that coolest word contains only letters"
  quickCheck prop_coolestWord
  putStrLn "Test coolWords :  check that returned list is sorted"
  quickCheck prop_coolWords
  putStrLn "Test coolWords :  check that returned list contains unique words"
  quickCheck prop_coolWordsUnique
  putStrLn "Test toLowerStrOnlyLetters"
  quickCheck prop_toLowerStrOnlyLetters
