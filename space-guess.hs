import System.Random (randomRIO)

-- Function to generate a random number between 1 and 100
randomNumber :: IO Int
randomNumber = randomRIO (1, 100)

-- Function to play the guessing game
playGame :: Int -> IO ()
playGame target = do
  putStrLn "Guess a number between 1 and 100:"
  guess <- getLine
  let guessInt = read guess :: Int
  if guessInt == target
    then putStrLn "Congratulations! You guessed the correct number."
    else do
      putStrLn (if guessInt < target then "Too low!" else "Too high!")
      playGame target

-- Main function to start the game
main :: IO ()
main = do
  putStrLn "Welcome to the Number Guessing Game!"
  putStrLn "I'm thinking of a number between 1 and 100."
  target <- randomNumber
  playGame target
