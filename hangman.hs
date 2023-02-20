import System.IO
import Text.Read   (readMaybe)
import System.Random 
-- I had to do stack install random in command to install the library, but check for your own computer


-- states adapted from MagicSum.hs

-- uncomment the {- to block out all the states
-- {-
data State = State InternalState [Action]  -- internal_state, available_actions
         deriving (Ord, Eq, Show)

        
data Result = EndOfGame Double State    -- end of game: value, starting state
            | ContinueGame State        -- continue with new state
         deriving (Eq, Show)

type Game = Action -> State -> Result

type Player = State -> Action



-- Actions for player
data Action = Letter Char
            | Word [Char]                   -- a move for a player is just Char or [Char]
         deriving (Ord,Eq)



-- "global" variables
type InternalState = ([Action], [Char], Int)   -- may need to be modified
-- letters guessed, word to guess, # of guesses

-- uncomment the -} to block out all the states
-- -}


-- function for word bank
wordBank :: [[Char]] 
wordBank = ["test", "words", "hangman"]

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
    


-- uncomment the {- to block out all the functions
-- {-



-- TODO:
-- start state function 
hangman_start = State ([]) [Action n | n <- [1..9]] --  change for letters
-- call generate_word to get the target word
-- use length word + 2 (?) for number of guesses
-- set the initial state



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

-- guessing a letter

letter_guess :: IO()
letter_guess = 
    do 
        putStrLn "Please enter a letter"
        line <- getLine -- check if line is a char
        --for loop through length of word 
        if line `elem` "word" 
            | print letter
            | print "-"
    
    -- build in option for if it is the last letter 
    -- call play again



-- guessing a word

word_guess :: IO()
word_guess = 
    do 
        putStrLn "Please enter a word"
        line <- getLine 
        if line == "word"
            return win -- figure out what win screen looks like
            return -- wrong guess, reduce one guess, call play again


--printing out a hint
print_hint :: IO()
print_hint = 
    putStrLn ++hint++
    call play again

-- uncomment the -} to block out all the functions
-- -}