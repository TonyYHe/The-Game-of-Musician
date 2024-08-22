-- Author:   Tony He <tyhe@student.unimelb.edu.au>
-- Purpose:  Plays the Game of Musician, which requires the performer to 
--           repeatedly guess the three-pitch chord given by the composer.
-------------------------------------------------------------------------------
-- * GAME RULES:
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
-- who would provide the following feedback:
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
-- * EXAMPLE:
--  Target  |  Guess   | Answer
-- A1,B2,A3 | A1,A2,B1 | 1,2,1
-- A1,B2,C3 | A1,A2,A3 | 1,0,2
-- A1,B1,C1 | A2,D1,E1 | 0,1,2
-- A3,B2,C1 | C3,A2,B1 | 0,3,3
-------------------------------------------------------------------------------
-- * PROGRAM FILE:
-- The program implements both the composer and performer parts of the game.
-- It utilizes a guess algorithm that attempts to minimize the number of 
-- guesses required to find the target.
-------------------------------------------------------------------------------
-- * APPROACH TO MAKING GUESSES:
-- The program uses the following method to find the target with fewest 
-- possible guesses:
--     (1) At each round, preserve potential targets that would provide the same
--         feedback as the previous guess, if now the previous guess is 
--         treated as the target and the candidate is the guess.
--     (2) Then, for each candidate compute the expected number of remaining 
--         possible targets if it is chosen as the next guess. 
--     (3) Finally, select the guess with the smallest expected number of 
--         remaining targets.
-------------------------------------------------------------------------------
-- (The above documentations reference the project specification written by 
--  Dr Peter Schachte)
-------------------------------------------------------------------------------

module Proj2 (Pitch, toPitch, feedback,
              GameState, initialGuess, nextGuess) where

import Data.List

-- | A GameState represents the list of remaining possible targets after each 
--   time the program receives feedback for a guess.
type GameState = [[Pitch]]
-- | A Note can be one of A, B, C, D, E, F, or G.
type Note = Char
noteVals = ['A'..'G']
-- | An Octave can be one of 1, 2, or 3.
type Octave = Char
octVals = ['1'..'3']

-- | A Pitch type, comprises a note and an octave.
data Pitch = Pitch Note Octave
    deriving Eq 
-- | Only display the components and not the data constructor to the terminal.
instance Show Pitch 
    where show (Pitch note octave) = [note, octave]

-- | Gives Just the Pitch named by the string, or Nothing if the string is 
--   not a valid pitch name.
toPitch :: String -> Maybe Pitch
toPitch [note, octave] 
    | note `elem` noteVals && octave `elem` octVals = Just (Pitch note octave) 
    | otherwise = Nothing
toPitch _ = Nothing
    
-- | Takes a target chord and a guess chord, respectively, and returns the 
--   appropriate feedback as mentioned in GAME RULES. The required statistics
--   are all computed by comparing the length of the guess chord/notes/octaves
--   before and after removing common elements with the target 
--   chord/notes/octaves.
feedback :: [Pitch] -> [Pitch] -> (Int, Int, Int)
feedback target guess = (correctPits, correctNots, correctOcts)
    where newTarget   = target \\ guess
          newGuess    = guess \\ target
          numPitsLeft = length newGuess
          correctPits = length guess - numPitsLeft
          correctNots = numPitsLeft - 
              length (getNotes newGuess \\ getNotes newTarget)
          correctOcts = numPitsLeft - 
              length (getOctaves newGuess \\ getOctaves newTarget)

-- | Takes a chord, extracts the note of each pitch, and outputs a list of 
--   notes
getNotes :: [Pitch] -> [Char]
getNotes chord = [n | Pitch n _ <- chord]

-- | Takes a chord, extracts the octave of each pitch, and outputs a list of 
--   octaves
getOctaves :: [Pitch] -> [Char]
getOctaves chord = [o | Pitch _ o <- chord]

-- | Takes no input arguments, and returns a pair of an initial guess and a 
--   game state. The initial guess is the output chord of the bestGuess function 
--   with the 1330 possible chords as input. The game state is simply the list 
--   of all possible chords excluding the initial guess. 
initialGuess :: ([Pitch], GameState)
initialGuess = (fstGuess, gameState)
    where fstGuess   = [Pitch 'A' '2', Pitch 'B' '1', Pitch 'C' '1']
          allPitches = [Pitch n o | n <- noteVals, o <- octVals]
          gameState  = filter ((==3).length) (subsequences allPitches) 
                       \\ [fstGuess]

-- | Takes as input a pair of the previous guess and game state, and the 
--   feedback to this guess, and returns a pair of the next guess and an updated
--   game state. The function filters the game state and preserves only 
--   potential targets that would have resulted in the same feedback as the 
--   previous guess. The next guess is then generated based on the updated game
--   state.
nextGuess :: ([Pitch], GameState) -> (Int, Int, Int) -> ([Pitch], GameState)
nextGuess (prevGuess, gameState) answer = 
    let candidates = [candidate 
                     | candidate <- gameState
                     , feedback prevGuess candidate == answer
                     ] \\ [prevGuess]
    in bestGuess candidates

-- | Takes as input a list of candidate chords and selects the candidate with 
--   the lowest score to be the next guess. The score refers to the expected 
--   number of remaining possible targets.
bestGuess :: [[Pitch]] -> ([Pitch], GameState)
bestGuess candidates = 
    let guessScores = [(guess, computeScore guess candidates) 
                      | guess <- candidates
                      ]
        (best, _)   = head (sortOn snd guessScores)
    in (best, candidates)

-- | Takes as input a guess and the candidate targets, and computes the expected 
--   number of remaining possible targets. The function does this by:
--   (1) compute all the feedbacks between the guess and the candidate targets
--   (2) count the number of occurrences for each type of feedback
--   (3) compute the average value, where 
--       x = number of occurrences of the feedback, 
--       average = sum of all (x*p(x)) 
computeScore :: [Pitch] -> [[Pitch]] -> Double
computeScore guess candidates = 
    sum [numAnswer*numAnswer/totAnswers 
        | n <- numAnswers
        , let numAnswer = fromIntegral n
        ]
    where feedbacks  = [feedback guess cand | cand <- candidates]
          numAnswers = map length ((group.sort) feedbacks)
          totAnswers = fromIntegral (sum numAnswers)