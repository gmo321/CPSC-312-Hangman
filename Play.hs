-- CPSC 312 - 2023 - Games in Haskell
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant bracket" #-}
module Play where

-- To run it, try:
-- ghci
-- :load Play
-- play hangman hangmanStart (0,0)

import Hangman
 -- importing our hangman file
import System.IO
import Text.Read   (readMaybe)



type TournammentState = (Int,Int)   -- wins, losses

play :: Game -> State -> TournammentState -> IO TournammentState

-- dislay tournament results
-- begin guessing
play game start_state ts =
  let (wins, losses) = ts in
  do
      putStrLn ("Tournament results: "++ show wins++ " wins "++show losses++" losses ")
      putStrLn "What would you like to do? 0 = start guessing, 1 = exit" 
      line <- getLine
      if line == "0"
        then
            do
                putStrLn ("Please enter a random number between 0 and " ++show (wBLength wordBank))
                line1 <- getLine
                case (readMaybe line1 :: Maybe Int) of
                    Nothing -> 
                        play game start_state ts
                    Just num ->  
                        do  
                            if num < 0 || num > (wBLength wordBank)
                                then 
                                    do 
                                putStrLn ("Number not in defined range.")
                                play game start_state ts
                            else 
                                person_play game (ContinueGame (generateWord start_state num)) ts
        else if line ==  "1"
            then return ts 
        else play game start_state ts

person_play :: Game -> Result -> TournammentState -> IO TournammentState
-- person is guessing a letter

person_play game (ContinueGame state) ts = 
  do
    putStrLn "What would you like to do? 0 = guess a letter, 1 = get a hint" 
    line <- getLine
    if line == "0"
      then letter_guess game (ContinueGame state) ts 
    else if line ==  "1"
        then print_hint game (ContinueGame state) ts
    else person_play game (ContinueGame state) ts 

-- end of game, tracking score                
person_play game (EndOfGame val start_state) ts =
  do
    new_ts <- update_tournament_state (val) ts  -- val is value to computer; -val is value for person
    play game start_state new_ts

-- guessing a letter
letter_guess :: Game -> Result -> TournammentState -> IO TournammentState
letter_guess game (ContinueGame state) ts = 
    do 
        let State (ltrs_guessed, word, guesses, hints) avail = state
        putStrLn("Number of guesses left: " ++ show guesses)
        -- Number of guesses is not decrementing in hangman function!
        -- hangman figure is also not changing when guess is wrong
        putStrLn("Please enter a letter in the alphabet wrapped in single quotations marks")
        ---- test what happens if they enter a letter not wrapped in single quotation mark
        input <- getLine
        case (readMaybe input :: Maybe Char) of
            Nothing ->
                letter_guess game (ContinueGame state) ts
            Just guess ->
                do
                    let lc_guess = toLower guess 
                    if (not(isAlphabet lc_guess)) -- if not a letter
                        then 
                            do
                                putStrLn("Please choose a letter in the alphabet.")
                                letter_guess game (ContinueGame state) ts
                    else if (lc_guess `elem` ltrs_guessed) -- if guessing a letter already guessed
                        then 
                            do
                                putStrLn("Please choose a letter that hasn't been chosen yet")
                                letter_guess game (ContinueGame state) ts
                    else 
                        do 
                            let print_word = word_str ltrs_guessed word lc_guess
                            putStrLn(print_word)
                            drawHangman (state)
                            person_play game (game (lc_guess) state) ts

-- printing a hint
print_hint :: Game -> Result -> TournammentState -> IO TournammentState
print_hint game (ContinueGame state) ts = 
    do
        let State (ltrs_guessed, word, guesses, hints) avail = state
        putStrLn ("You have " ++show hints++ " hints left. Press 0 for a hint.")
        line <- getLine
        if (line == "0")
            then
                do 
                    if (hints == 3)
                        then 
                            do
                                putStrLn ("The total number of vowels in the word are " ++show (num_vowels word))
                                person_play game (ContinueGame (updateHint state)) ts 
                    else if (hints > 0)
                        then 
                            do 
                                putStrLn ("The next missing letter is:" ++[reveal_letter word ltrs_guessed])
                                person_play game (ContinueGame (updateHint state)) ts
                    else 
                        do 
                            putStrLn "There are no hints left."
                            person_play game (ContinueGame (updateHint state)) ts

        else 
            do 
                putStrLn "Incorrect input." 
                print_hint game (ContinueGame state) ts    




update_tournament_state:: Double -> TournammentState -> IO TournammentState
-- given value to the person, the tournament state, return the new tournament state
update_tournament_state val (wins,losses)
  | val == 1 = do
      putStrLn "You won!"
      return (wins+1,losses)
  | val == 0 = do
      putStrLn "You lost!"
      return (wins,losses+1)

