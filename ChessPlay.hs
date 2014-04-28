module ChessPlay where

import System.IO

import ChessData
import ChessText
import ChessRules

type Input m = Game -> m Move
type Output m = Game -> m ()

play :: Monad m => (Input m, Input m) -> Output m -> m (Game, Result)
play = play' initialGame

play' :: Monad m => Game -> (Input m, Input m) -> Output m -> m (Game, Result)
play' game (in1, in2) out = do
    out game
    move <- in1 game
    let game' = doMoveGame move game
    case result game' of
        Nothing -> play' game' (in2, in1) out
        Just r  -> return (game', r)
