{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
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

type Player = State -> Action

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
          ContinueGame (State (letters_guessed, word, guesses-1)   -- note roles have flipped
                        [act | act <- available, act /= move])


-- {-

-- main print out at start
play :: IO()
play = do 
    --putStrLn ("Tournament results: "++ show wins++ " wins "++show losses++" losses "++show ties++" ties")
    putStrLn "What would you like to do? 0 = guess a letter, 1 = guess a word, 2 = get a hint, 3 = exit" 
    line <- getLine
    if line == "0"
        then
            letter_guess
        else if line ==  "1"
             then word_guess
        else if line == "2"
            then print_hint
        else if line == "3"
            then return quit
        else "illegal"

-- letter_guess game (EndOfGame value start_state) ts

-- guessing a letter
-- person_play in Play.hs?
-- put in Play.hs?
-- need Player?
letter_guess :: Game -> Result -> TournammentState -> IO TournammentState
letter_guess game (ContinueGame state) ts = 
    do 
        let State (ltrs_guessed, word, guesses) avail = state
        putStrLn("Please enter a letter in the Alphabet wrapped in single quotations marks")
        input <- getChar 
        if (not(isAlphabet input)) 
            then 
                do
                putStrLn("Please choose a letter in the Alphabet")
                letter_guess game (ContinueGame state) ts
        else if (input `elem` ltrs_guessed)
            then 
                do
                putStrLn("Please choose a letter that hasn't been chosen yet")
                letter_guess game (ContinueGame state) ts
        else 
            do 
            let print_word = word_str ltrs_guessed word i
            putStrLn(print_word)
            -- update letters_guessed here or in hangman?
            -- keep track of scores?
    
    -- build in option for if it is the last letter 
    -- call play again

-- returns a string, displaying the letter if guessed correctly and dashes if not, 
-- and dashes for rest of letters not guessed, and displaying the letters guessed correctly before
word_str :: [Char] -> Char -> [Char] -> [Char]
word_str ltrs_g ans l  = [if (x == l || x `elem` ltrs_g ) then x else '_' | x <- ans]

-- checks if input is an alphabet letter
isAlphabet :: Char -> Bool
isAlphabet i = i `elem` "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"




--printing out a hint
print_hint :: IO()
print_hint = 
    putStrLn ++hint++
    call play again

-- -}