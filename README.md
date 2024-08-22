# The-Game-of-Musician
Plays the Game of Musician, which requires the performer to repeatedly guess the 
three-pitch chord given by the composer.

## Game Rules
The composer begins by selecting a three-pitch musical chord, referred to 
as the target, where each pitch comprises:
1. a musical note (one of A, B, C, D, E, F, or G)
2. an octave (one of 1, 2, or 3)

The target has the following additional properties:
1. The order of pitches is irrelevant
2. No pitch may appear more than once
3. The chord doesn't include sharps or flats

Once the composer has selected the target chord, the performer repeatedly 
chooses a similarly defined chord as a guess and tells it to the composer, 
who would provide the following feedback:
1. the number of matching pitches
2. the number of correct notes
3. the number of correct octaves

Note that in counting correct notes and octaves, multiple occurrences in the 
guess are only counted as correct if they also appear repeatedly in the 
target. Correct pitches are not also counted as correct notes and octaves.

The game finishes once the performer guesses the correct chord (all three 
pitches in the guess are in the target). The object of the game for the 
performer is to find the target with the fewest possible guesses.

## Example
 Target  |  Guess   | Answer
-------- | -------- | --------
A1,B2,A3 | A1,A2,B1 | 1,2,1
A1,B2,C3 | A1,A2,A3 | 1,0,2
A1,B1,C1 | A2,D1,E1 | 0,1,2
A3,B2,C1 | C3,A2,B1 | 0,3,3

## Program File
The program implements both the composer and performer parts of the game.
It utilizes a guess algorithm that attempts to minimize the number of 
guesses required to find the target.

## Approach to Making Guesses
The program uses the following method to find the target with fewest 
possible guesses:
1. At each round, preserve potential targets that would provide the same 
feedback as the previous guess, if now the previous guess is treated as the 
target and the candidate is the guess.
2. Then, for each candidate compute the expected number of remaining possible
targets if it is chosen as the next guess. 
3. Finally, select the guess with the smallest expected number of remaining 
targets.