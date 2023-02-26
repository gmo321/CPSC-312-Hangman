{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant bracket" #-}
module Hangman where
import System.IO
import Text.Read   (readMaybe)


-- adapted from MagicSum.hs
data State = State InternalState [Char]  -- internal_state, available_actions
         deriving (Ord, Eq, Show)

        
data Result = EndOfGame Double State    -- end of game: value, starting state
            | ContinueGame State        -- continue with new state
         deriving (Eq, Show)

type Game = Char -> State -> Result

-- type Player = State -> Action -- I don't think we need Player, it's used as an "opponent" in MagicSum

-- Actions for player
data Action = Letter Char  -- a move for a player is just Char
         deriving (Ord,Eq)


-- "global" variables
type InternalState = ([Char], [Char], Int, Int)   -- may need to be modified
-- (letters guessed, word to guess, # of guesses left, # of hints left)

----- Word Bank Functions ---

-- function for word bank
-- filters out words less than 4 letters since we have 6 guesses
wordBank :: [[Char]] 
wordBank = 
    do
        let wlist = ["test", "words", "hangman", "two"] -- list of words
        filter (\x -> length x >= 4) wlist -- filter

-- function for finding length of word bank
wBLength :: [t] -> Int
wBLength [] = 0
wBLength (h:t) =  1 + wBLength t



----- Hangman Start State Function -----

-- empty list for letters guessed so far
-- empty word 
-- start wth 6 guesses since there is only six body parts
-- start with 3 hints to use
-- set the initial state

hangmanStart :: State
hangmanStart = State ([], "", 6, 3) ['a'..'z']  --  change for letters

generateWord :: State -> Int -> State
generateWord (State (ltrs_guessed, word, guesses, hints) avail) num = 
    State (ltrs_guessed, wordBank !! num, guesses, hints) avail

----- Hangman Game ------
hangman :: Game
hangman move (State (ltrs_guessed, word, guesses, hints) available) 
    | win move word ltrs_guessed          = EndOfGame 1  hangmanStart     -- player wins
    | guesses==1                          = EndOfGame 0  hangmanStart     -- no more guesses, player loses
    | move `elem` word               = 
        ContinueGame (State ((move:ltrs_guessed), word, guesses, hints)   -- correct guess, number of guess is not reduced
                        [act | act <- available, act /= move])
    | otherwise                           =
          ContinueGame (State ((move:ltrs_guessed), word, guesses - 1, hints)   -- reduce a guess
                        [act | act <- available, act /= move])

                        
-- takes in move, the answer, the letters guessed and returns true if word_str matches ans
win :: Char -> [Char] -> [Char] -> Bool
win move word ltrs_guessed =  and [ x `elem` move:ltrs_guessed | x <- word]

    

----- Helper Functions -----


-- returns a string, displaying the letter if guessed correctly and dashes if not, 
-- and dashes for rest of letters not guessed, and displaying the letters guessed correctly before
word_str :: [Char] -> [Char] -> Char -> [Char]
word_str ltrs_g ans l  = [if (x == l || x `elem` ltrs_g ) then x else '_' | x <- ans]

-- checks if input is an alphabet letter
isAlphabet :: Char -> Bool
isAlphabet i = i `elem` "abcdefghijklmnopqrstuvwxyz"

-- Citation: https://www.cs.ubc.ca/~poole/cs312/2023/as2/As2sol.hs
-- if a character is an upper-case letter, returns the lower-case letter, otherwise remain unchanged
toLower :: Char -> Char
toLower x 
    | x `elem` "ABCDEFGHIJKLMNOPQRSTUVWXYZ" = toEnum(fromEnum x + fromEnum 'a' - fromEnum 'A')
    | otherwise = x




-- to update # of hints in internal state
updateHint :: State -> State
updateHint (State (ltrs_guessed, word, guesses, hints) avail) = 
    State (ltrs_guessed, word, guesses, hints - 1) avail



-- TODO: restrict user from getting hint with 1 letter left?
-- Takes the answer and letters already guessed and returns the first letter that is in word and has not been guessed yet

--- repeated letters????
reveal_letter :: [Char] -> [Char] -> Char
reveal_letter ans ltrs_g = 
    head [c | c <- ans, c `notElem` ltrs_g] 


-- checks if char is a vowel
isVowel :: Char -> Bool 
isVowel x = x `elem` "aeiou"

-- returns num of vowels in string
num_vowels :: [Char] -> Int 
num_vowels = length . filter isVowel

----- Drawing the hangman -----
drawHangman:: State -> IO()
drawHangman (State (ltrs_guessed, word, guesses, hints) avail) 
    | guesses == 6   = sixGuesses
    | guesses == 5   = fiveGuesses
    | guesses == 4   = fourGuesses
    | guesses == 3   = threeGuesses
    | guesses == 2   = twoGuesses
    | guesses == 1   = oneGuess
    | guesses == 0   = zeroGuesses


----- functions to print the hangman based on guesses -----

-- 0 wrong guesses, 6 guesses left        
sixGuesses:: IO()
sixGuesses = 
    do
        putStrLn("+-----------+")
        putStrLn("|           |")
        putStrLn("|           ")
        putStrLn("|          ")
        putStrLn("|          ")
        putStrLn("=")

-- 1 wrong guesses, 5 guesses left        
fiveGuesses:: IO()
fiveGuesses = 
    do
        putStrLn("+-----------+")
        putStrLn("|           |")
        putStrLn("|           O")
        putStrLn("|          ")
        putStrLn("|          ")
        putStrLn("=")

-- 2 wrong guesses, 4 guesses left        
fourGuesses:: IO()
fourGuesses = 
    do
        putStrLn("+-----------+")
        putStrLn("|           |")
        putStrLn("|           O")
        putStrLn("|           |")
        putStrLn("|          ")
        putStrLn("=")

-- 3 wrong guesses, 3 guesses left        
threeGuesses:: IO()
threeGuesses = 
    do
        putStrLn("+-----------+")
        putStrLn("|           |")
        putStrLn("|           O")
        putStrLn("|          \\|")
        putStrLn("|          ")
        putStrLn("=")

-- 4 guesses, 2 guesses left        
twoGuesses:: IO()
twoGuesses = 
    do
        putStrLn("+-----------+")
        putStrLn("|           |")
        putStrLn("|           O")
        putStrLn("|          \\|/")
        putStrLn("|          ")
        putStrLn("=")

-- 5 guesses, 1 guess left        
oneGuess:: IO()
oneGuess = 
    do
        putStrLn("+-----------+")
        putStrLn("|           |")
        putStrLn("|           O")
        putStrLn("|          \\|/")
        putStrLn("|          / ")
        putStrLn("=")


-- 6 guesses, 0 guesses left        
zeroGuesses:: IO()
zeroGuesses = 
    do
        putStrLn("+-----------+")
        putStrLn("|           |")
        putStrLn("|           O")
        putStrLn("|          \\|/")
        putStrLn("|          / \\")
        putStrLn("=")

