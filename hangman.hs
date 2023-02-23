{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use camelCase" #-}
module Hangman where
import System.IO
import Text.Read   (readMaybe)
import System.Random 
-- I had to do stack install random in command to install the library, but check for your own computer

-- adapted from MagicSum.hs

data State = State InternalState [Action]  -- internal_state, available_actions
       --  deriving (Ord, Eq, Show)

        
data Result = EndOfGame Double State    -- end of game: value, starting state
            | ContinueGame State        -- continue with new state
        -- deriving (Eq, Show)

type Game = Action -> State -> Result

-- type Player = State -> Action -- I don't think we need Player, it's used as an "opponent" in MagicSum

-- Actions for player
data Action = Letter Char  -- a move for a player is just Char
        -- deriving (Ord,Eq)


-- "global" variables
type InternalState = ([Action], IO [Char], Int)   -- may need to be modified
-- (letters guessed, word to guess, # of guesses left)


-- function for word bank
-- filters out words less than 4 letters since we have 6 guesses
wordBank :: [[Char]] 
wordBank = 
    do
        let wlist = ["test", "words", "hangman", "two"] -- list of words
        filter (\x -> length x >= 4) wlist -- filter

-- function for finding length of word bank
-- because length doesn't work on a nested list as I PAINFULLY found out
wBLength :: [t] -> Int -> Int
wBLength [] x = x 
wBLength (h:t) x =  wBLength t x+1 


-- Citation: https://stackoverflow.com/questions/25923686/get-a-random-list-item-in-haskell
-- function to return a word from the word bank
generateWord :: IO [Char]
generateWord = 
    do
    let length = wBLength wordBank 0 
    num <- randomRIO (0, length - 1) -- random number generator 
    return (wordBank !! num)
    

-- start state function
-- empty list for letters guessed so far
-- call generate_word to get the target word
-- start wth 6 guesses since there is only six body parts
-- set the initial state

hangmanStart :: State
hangmanStart = State ([], generateWord, 6) [Letter n | n <-['a'..'z']]  --  change for letters

--TODO:
-- remove ties from tournament state
-- write win function 
hangman :: Game
hangman move (State (letters_guessed, word, guesses) available) 
    | win move word                = EndOfGame 1  hangmanStart     -- player wins
    | guesses==0                   = EndOfGame 0  hangmanStart     -- no more guesses, player loses
    | otherwise                    =
          ContinueGame (State (letters_guessed, word, guesses-1)   -- reduce a guess
                        [act | act <- available, act /= move])

-- letters_guessed might need to be edited in the third option to include the move

-- {-


-- returns a string, displaying the letter if guessed correctly and dashes if not, 
-- and dashes for rest of letters not guessed, and displaying the letters guessed correctly before
word_str :: [Char] -> [Char] -> Char -> [Char]
word_str ltrs_g ans l  = [if (x == l || x `elem` ltrs_g ) then x else '_' | x <- ans]

-- checks if input is an alphabet letter
isAlphabet :: Char -> Bool
isAlphabet i = i `elem` "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

{-
-- guessing a word

word_guess :: IO()
word_guess = 
    do 
        putStrLn "Please enter a word"
        line <- getLine 
        if line == "word"
            return win -- figure out what win screen looks like
            return wrong_guess-- wrong guess, reduce one guess, call play again
-}

-- return if all letters in word are guessed; Player wins
win :: Char -> [Char] -> 
win = 
    do 
        putStrLn "You guessed correctly! The word is " ++ show_word
        putStrLn "Play again = 0 or quit = 1"
        line <- getLine
        if line == 0
            return play
            else if line == 1
                then return ts

-- TODO
-- return if letter guessed is not in word or word guessed is incorrect
wrong_guess :: Char -> [Char] ->  
wrong_guess = putStrLn "You guessed incorrectly, guess again!"
    return play

--printing out a hint
print_hint :: IO()
print_hint = 
    do 
        putStrLn ++hint++
        call play again


hint :: [Char] -> [Char]
-- return hint:
    --number of vowels in word
    --check if word has letter == "aeiou", if true, return length 
    --reveal one letter in word?

    -- if remaining letters are not in [letter_guessed] return first letter in list
    
first_hint = putStrLn "The number of vowels in the word are " ++ num_vowels

second_hint = reveal_letter

third_hint = reveal_letter


-- TODO
reveal_letter :: [Char] -> [Char]
reveal_letter word
    | if (h == "-") 
        then print head 


-- TODO
show_word :: [Char] -> [Char]
show_word word = putStrLn word

-- TODO
num_vowels :: [Char] -> Int
num_vowels word = 
    | if elem in word == "aeiou"
        then return length word
        else putStrLn "This word contains no vowels."


-- uncomment the -} to block out all the functions
-- -}