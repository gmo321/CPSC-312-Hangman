-- CPSC 312 - 2023 - Games in Haskell
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant bracket" #-}
module Play where

-- To run it, try:
-- ghci
-- :load Play

import MagicSum
--import CountGame

--import Minimax  -- make sure same game is imported in Minimax
--import Minimax_mem

import Hangman -- importing our hangman file
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
            person_play game (ContinueGame start_state) ts
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
        then return print_hint 
    else person_play game (ContinueGame state) ts 

-- letter_guess game (EndOfGame value start_state) ts

-- guessing a letter
-- person_play in Play.hs?
-- need Player? (Carmen: no, we don't really have an opponent so no need for Player)     
letter_guess :: Game -> Result -> TournammentState -> IO TournammentState
letter_guess game (ContinueGame state) ts = 
    do 
        let State (ltrs_guessed, word, guesses, hints) avail = state
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
        
        
            -- update letters_guessed here or in hangman? (Carmen:I think update here)
            -- keep track of scores? (That's done via TournamentState so I think we're good)
            -- Carmen: I think we need to call person_play again at the end just to make sure it loops after it prints the word
    
    -- build in option for if it is the last letter 
    -- call play again
      

-- end of game, tracking score                
person_play game (EndOfGame val start_state) opponent ts =
  do
    new_ts <- update_tournament_state (-val) ts  -- val is value to computer; -val is value for person
    play game start_state opponent new_ts



update_tournament_state:: Double -> TournammentState -> IO TournammentState
-- given value to the person, the tournament state, return the new tournament state
update_tournament_state val (wins,losses)
  | val == 1 = do
      putStrLn "You won!"
      return (wins+1,losses)
  | val == 0 = do
      putStrLn "You lost!"
      return (wins,losses+1)


-- If you imported MagicSum here and in Minimax try:
-- play magicsum magicsum_start simple_player (0,0,0)
-- play magicsum magicsum_start (mm_player magicsum) (0,0,0) -- minimax player

-- If you imported CountGameNew here and in Minimax_mem try:
-- let (cg, ss) = createCountGame 20 [1,2,3,5,7] in play cg ss (simple_count_player 20 [1,2,3,5,7]) (0,0,0)
-- let (cg, ss) = createCountGame 20 [1,2,3,5,7] in play cg ss (mm_player cg) (0,0,0)