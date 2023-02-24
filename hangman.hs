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
type InternalState = ([Action], IO [Char], Int, Int)   -- may need to be modified
-- (letters guessed, word to guess, # of guesses left, # of hints left)


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
-- start with 3 hints to use
-- set the initial state

hangmanStart :: State
hangmanStart = State ([], generateWord, 6, 3) [Letter n | n <-['a'..'z']]  --  change for letters

--TODO:
-- remove ties from tournament state
-- write win function 
hangman :: Game
hangman move (State (letters_guessed, word, guesses, hints) available) 
    | win move word                = EndOfGame 1  hangmanStart     -- player wins
    | guesses==0                   = EndOfGame 0  hangmanStart     -- no more guesses, player loses
    | otherwise                    =
          ContinueGame (State (letters_guessed, word, guesses-1, hints)   -- reduce a guess
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


-- TODO
-- function called when all letters in word are guessed; Player wins
win :: Char -> [Char] -> Result
win move word = 
    do 
        putStrLn "You guessed the word correctly! The word is " ++ word

-- anything else needed in win?
-- can i just display the word like this?


-- TODO: test
-- takes ans and returns IO output
-- return hint:
    --number of vowels in word
    --check if word has letter == "aeiou", if true, return length 
    --reveal one letter in word?
    -- if remaining letters are not in [letter_guessed] return first letter in list
print_hint :: [Char] Int -> IO()
print_hint word hints = 
    do 
        putStrLn "You get three hints. Press 0 for a hint."
        line <- getLine
        if (line == "0")
            then subtract 1 hints
                do 
                if (hints == 3)
                    then return putStrLn "The total number of vowels in the word are " ++ num_vowels
                else if (hints > 0)
                    then reveal_letter
                else putStrLn "There are no hints left."
            return letter_guess game (ContinueGame state) ts

-- TODO: restrict user from getting hint with 1 letter left?
-- Takes the answer and letters already guessed and returns the first letter that is in word and has not been guessed yet
reveal_letter ::  Eq a => [a] -> [a] -> a
reveal_letter word ltrs_g = head [c | c <- word, c `notElem` ltrs_g] 


-- checks if char is a vowel
isVowel :: Char -> Bool 
isVowel x = x `elem` "aeiou"

-- returns num of vowels in string
num_vowels :: [Char] -> Int 
num_vowels = length . filter isVowel

-- uncomment the -} to block out all the functions
-- -}