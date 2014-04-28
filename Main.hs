import System.IO

import ChessData
import ChessPlay
import ChessText

main = do
    (_, result) <- play (human, human) printGame
    case result of
      Checkmate{rWinner=wc} -> putStrLn (show wc ++ " wins by checkmate.")
      Stalemate             -> putStrLn ("The game is drawn by stalemate.")

human :: Input IO
human game@Game{gTurn=pc} = do
    putStr (show pc ++ "> ")
    hFlush stdout
    line <- getLine
    case readMove game line of
      Left error -> do
        putStrLn ("Error: " ++ error ++ ".")
        human game
      Right move -> do
        putStrLn ""
        return move
