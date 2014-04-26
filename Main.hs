import System.IO

import ChessData
import ChessText
import ChessRules

main = play initialGame

play :: Game -> IO ()
play game = do
    printBoard (gBoard game)
    play' game

play' :: Game -> IO ()
play' game = do
    putStr $ show (gTurn game) ++ "> "
    hFlush stdout
    line <- getLine
    case readMove game line of
      Left error -> do
        putStrLn $ "Error: " ++ error ++ "."
        play' game 
      Right move -> do
        putStrLn ""
        play $ doMoveGame move game
