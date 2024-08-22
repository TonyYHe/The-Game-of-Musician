--  Author   : Peter Schachte
--  Purpose  : Test program
--  Copyright: (c) 2020 The University of Melbourne

-- TESTING CODE.  DO NOT EDIT.

module Main where

import Data.List
import Data.Maybe
import Proj2
import System.Environment
import System.Exit


-- | Guess the given target, counting and showing the guesses.
guessTest :: [Pitch] -> IO ()
guessTest target = do
      let (guess,other) = initialGuess
      loop target guess other 1


-- | Given a target and guess and a guess number, continue guessing
-- until the right target is guessed.
loop :: [Pitch] -> [Pitch] -> GameState -> Int -> IO ()
loop target guess other guesses = do
  putStrLn $ "Your guess #" ++ show guesses ++ ":  " ++ show guess
  let answer = feedback target guess
  putStrLn $ "    My answer:  " ++ show answer
  if answer == (3,0,0)
    then do
      putStrLn $ "You got it in " ++ show guesses ++ " guesses!"
    else do
      let (guess',other') = nextGuess (guess,other) answer
      loop target guess' other' (guesses+1)


-- | Parse a string containing a number of space-separated pitches to produce
-- a list of pitches.  Error if any of the pitches can't be parsed.
toChord :: String -> [Pitch]
toChord = (fromJust . mapM toPitch . words)


-- | Prompt for a target and use guessTest to try to guess it.
main :: IO ()
main = do
  args <- getArgs
  -- let target = args
  -- let test = head args
  -- if length args == 3 then do
  --   let (guess,other) = initialGuess
  --   loop target guess other 1
  --   else do
  --   putStrLn "Usage:  proj1 p1 p2 p3"
  --   putStrLn "   where p1 p2 p3 are 3 different pitches between A1 and G3"
  --   exitFailure
  guessTest $ toChord (intercalate " " args)

-- main :: IO ()
-- main = do
--   putStr "Target chord (3 pitches separated by spaces): "
--   text <- getLine
--   guessTest $ toChord text