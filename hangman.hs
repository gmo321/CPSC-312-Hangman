import System.IO
import Text.Read   (readMaybe)

-- word bank 
-- function that selections a word 
-- internal variable for # of guesses

-- main print out at start
play :: IO()
play = do 
    -- putStrLn ("Tournament results: "++ show wins++ " wins "++show losses++" losses "++show ties++" ties")
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
    putStrLn "Please enter a letter"
    line <- getLine -- check if line is a char
    -- for loop through length of word 
    if line `elem` "word" 
        | print letter
        | print "-"
    
    -- build in option for if it is the last letter 
    -- call play again



-- guessing a word

word_guess :: IO()
word_guess = 
    putStrLn "Please enter a word"
    line <- getLine 
    if line == "word"
        return win -- figure out what win screen looks like
        return -- wrong guess, reduce one guess, call play again


-- printing out a hint
print_hint :: IO()
print_hint = 
    putStrLn ++hint++
    -- call play again