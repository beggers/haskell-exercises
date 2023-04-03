module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.IO (BufferMode(NoBuffering),
                  hSetBuffering,
                  stdout)
import System.Random (randomRIO)

type WordList = [String]

data Puzzle = Puzzle String [Maybe Char] [Char]
instance Show Puzzle where
    show (Puzzle _ discovered guessed) =
        (intersperse ' ' $ fmap renderPuzzleChar discovered)
        ++ " Guessed so far: " ++ guessed

minWordLength :: Int
minWordLength = 5
maxWordLength :: Int
maxWordLength = 10

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    word <- randomWord'
    let puzzle = freshPuzzle (fmap toLower word)
    runGame puzzle

freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s (map (const Nothing) s) []

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
    gameWin puzzle
    gameOver puzzle
    putStrLn $
        "Current puzzle is: " ++ show puzzle
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
        [c] -> handleGuess puzzle c >>= runGame
        _ ->
            putStrLn "Your guess must be a single character."

gameOver :: Puzzle -> IO()
gameOver (Puzzle w _ gs) =
    if (length gs) > 7 then
        do putStrLn "You lose!"
           putStrLn $ "The word was: " ++ w
           exitSuccess
    else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ soFar _) =
    if all isJust soFar then
        do putStrLn "You win!!!!!!!!!!!!"
           exitSuccess
    else return ()

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
    putStrLn $ "Your guess was: " ++ [guess]
    case (charInWord puzzle guess
         , alreadyGuessed puzzle guess) of
        (_, True) -> do
            putStrLn "You already guessed that character, pick something else!"
            return puzzle
        (True, _) -> do
            putStrLn "This character was in the word,\
                \ filling in the word accordingly."
            return (fillInCharacter puzzle guess)
        (False, _) -> do
            putStrLn "This character wasn't in the word, try again!"
            return (fillInCharacter puzzle guess)

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle w _ _ ) c = elem c w

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ g) c = elem c g

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

randomWord :: WordList -> IO String
randomWord wl = do
    randomIndex <- randomRIO (0, (length wl) - 1)
    return $ wl !! randomIndex

gameWords :: IO WordList
gameWords = do
    aw <- allWords
    return (filter gameLength aw)
    where gameLength w =
            let l = length (w :: String)
            in      l >= minWordLength
                && l < maxWordLength

allWords :: IO WordList
allWords = do
    dict <- readFile "data/words"
    return (lines dict)

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word soFar s) c =
    Puzzle word newSoFar (c : s)
    where zipper guessed wordChar guessChar =
            if wordChar == guessed
            then Just wordChar
            else guessChar
          newSoFar = zipWith (zipper c) word soFar

