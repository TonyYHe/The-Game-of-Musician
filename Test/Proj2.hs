-- Author:   Tony He <tyhe@student.unimelb.edu.au>
-- Purpose:  Plays the Game of Musician, which requires the performer to 
--           repeatedly guess the three-pitch chord given by the composer.
-------------------------------------------------------------------------------
-- GAME RULES:
-- The composer begins by selecting a three-pitch musical chord, referred to 
-- as the target, where each pitch comprises:
--     (1) a musical note (one of A, B, C, D, E, F, or G)
--     (2) an octave (one of 1, 2, or 3)
--
-- The target has the following additional properties:
--     (1) The order of pitches is irrelevant
--     (2) No pitch may appear more than once
--     (3) The chord doesn't include sharps or flats
-- 
-- Once the composer has selected the target chord, the performer repeatedly 
-- chooses a similarly defined chord as a guess and tells it to the composer, 
-- who would provdide the following feedback:
--     (1) the number of matching pitches
--     (2) the number of correct notes
--     (3) the number of correct octaves
-- 
-- Note that in counting correct notes and octaves, multiple occurrences in the 
-- guess are only counted as correct if they also appear repeatedly in the 
-- target. Correct pitches are not also counted as correct notes and octaves.
--
-- The game finishes once the performer guesses the correct chord (all three 
-- pitches in the guess are in the target). The object of the game for the 
-- performer is to find the target with the fewest possible guesses.
-------------------------------------------------------------------------------
-- EXAMPLE:
--  Target  |  Guess   | Answer
-- A1,B2,A3 | A1,A2,B1 | 1,2,1
-- A1,B2,C3 | A1,A2,A3 | 1,0,2
-- A1,B1,C1 | A2,D1,E1 | 0,1,2
-- A3,B2,C1 | C3,A2,B1 | 0,3,3
-------------------------------------------------------------------------------
-- PROGRAM FILE:
-- The program implements both the composer and performer parts of the game.
-------------------------------------------------------------------------------
-- APPROACH TO MAKING GUESSES:
-- The program uses the following method to find the target with fewest 
-- possible guesses:
--     (1) At each round, preserve potential pitches that would provide the same
--         feedback as the previous guess, if now the previous guess is 
--         treated as the target and the candidate is the guess
--     (2) Then, for each candidate compute the expected number of remaining 
--         possible targets if it is chosen as the target. 
--     (3) Finally, select the guess with the smallest expected number of 
--         remaining targets
-------------------------------------------------------------------------------
-- (The above documentations reference the project specification written by 
--  Dr Peter Schachte)
-------------------------------------------------------------------------------

module Proj2 (Pitch, toPitch, feedback,
              GameState, initialGuess, nextGuess) where
              
import Data.List

-- | A GameState stores the list of remaining possible targets after each time 
--   the program recieves feedback for a guess
type GameState = [[Pitch]]
-- | A Note can be one of A, B, C, D, E, F, or G
type Note = Char
noteVals = ['A'..'G']
-- | An Octave can be one of 1, 2, or 3
type Octave = Char
octVals = ['1'..'3']

-- | A Pitch type, comprises a note and octave
data Pitch = Pitch Note Octave
    deriving Eq 
-- | When printing a pitch to the terminal, we exclude the data constructor and 
--   only print the components
instance Show Pitch 
    where show (Pitch note octave) = [note, octave]

-- | Gives Just the Pitch named by the string, or Nothing if the string is not a 
--   valid pitch name.
toPitch :: String -> Maybe Pitch
toPitch [note, octave] 
    | note `elem` noteVals && octave `elem` octVals = Just (Pitch note octave) 
    | otherwise = Nothing
toPitch _ = Nothing
    
-- | Takes a target chord and a guess chord, respectively, and returns the 
--   appropriate feedback as mentioned in GAME RULES. The function defines and 
--   uses two helper functions "nots" and "octs", which returns the list of 
--   notes and octaves in the input chord, respectively.
feedback :: [Pitch] -> [Pitch] -> (Int,Int,Int)
feedback target guess = (correctPits, correctNots, correctOcts)
    where newTarget   = target \\ guess
          newGuess    = guess \\ target
          newGuessLen = length newGuess
          correctPits = length guess - newGuessLen
          correctNots = newGuessLen - length (nots newGuess \\ nots newTarget)
          correctOcts = newGuessLen - length (octs newGuess \\ octs newTarget)
          nots chord = [n | Pitch n _ <- chord]
          octs chord = [o | Pitch _ o <- chord]

-- | Takes no input arguments, and returns a pair of an initial guess and a 
--   game state. The initial guess was found by iterating through all possible
--   guesses, running the program against all 1330 possible chords, compute the 
--   average number of guesses for each guess, and select the one with the 
--   lowest score.
initialGuess :: ([Pitch],GameState)
initialGuess = (fstGuess, gameState)
    where fstGuess   = [Pitch 'A' '2', Pitch 'B' '1', Pitch 'C' '1']
          posPitches = [Pitch n o | n <- noteVals, o <- octVals]
          gameState  = filter ((==3).length) (subsequences posPitches) 
                       \\ [fstGuess]

-- | Takes as input a pair of the previous guess and game state, and the 
--   feedback to this guess, and returns a pair of the next guess and game 
--   state. 
--   
--   ALGORITHM:
--   (1) The function first identifies possible targets that are
--   consistent with the input feedback (which happens to be the answer 
--   received for the previous guess) if the feedback you would receive for 
--   that guess and that target is is identical to answer you actually received 
--   for that guess. 
--   (2) For each candidate compute the expected number of remaining possible 
--   targets if it is chosen as the target. 
--   (3) Finally, select the candidate with the smallest expected number of 
--    remaining targets, and it shall be the next guess
nextGuess :: ([Pitch],GameState) -> (Int,Int,Int) -> ([Pitch],GameState)
nextGuess (prevGuess, gameState) answer = 
    let candidates = [posTarget 
                     | posTarget <- gameState
                     , feedback prevGuess posTarget == answer
                     ] \\ [prevGuess]
    in bestGuess candidates

-- | Takes as input a list of candidate pitches and selects the candidate with 
--   the lowest score to be the next guess. The score refers to the expected 
--   number of remaining possible targets for each guess. 
bestGuess :: [[Pitch]] -> ([Pitch],GameState)
bestGuess candidates = 
    let guessScores = [(guess, computeScore guess candidates) | 
                      guess <- candidates]
        (best, _)   = head (sortOn snd guessScores)
    in (best, candidates)

-- | Takes as input a guess and the candidate chords, and computes the expected 
--   number of remaining possible targets. The function does this by:
--   (1) compute all the feedbacks between the guess and the candidates
--   (2) count the number of occurrences for each type of feedback
--   (3) compute the expected value
computeScore :: [Pitch] -> [[Pitch]] -> Double
computeScore guess candidates = 
    sum [numAnswer*numAnswer/totAnswers 
        | n <- numAnswers
        , let numAnswer = fromIntegral n
        ]
    where feedbacks = [feedback guess cand | cand <- candidates]
          numAnswers = map length ((group.sort) feedbacks)
          totAnswers = fromIntegral (sum numAnswers)

posPitches = [Pitch n o | n <- noteVals, o <- octVals]
gameState  = filter ((==3).length) (subsequences posPitches) 