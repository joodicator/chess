--------------------------------------------------------------------------------
module ChessControl where

import Control.Applicative
import Data.Char
import Data.List

import ChessData
import ChessRules
import ChessText
import Multiplex

--------------------------------------------------------------------------------
gameChannel :: NameChan ()
gameChannel = do
    (mn,line) <- readChan
    case line of
        "!start" -> gameChannelStart
        _        -> gameChannel

gameChannelStart :: NameChan ()
gameChannelStart = do
    gameChannelPlay playName

gameChannelPlay :: NameChan () -> NameChan ()
gameChannelPlay subChan = do
    subChan <- takeSubChan subChan
    (mn,line) <- readChan
    case line of
        "!stop" -> do
            gameChannelEnd
            gameChannel
        "!restart" -> do
            gameChannelEnd
            gameChannelStart
        _ | take 1 line == "!" -> do
            gameChannelPlay subChan
        _ | otherwise -> do
            subChan <- feedSubChan (mn,line) subChan
            gameChannelPlay subChan 

gameChannelEnd :: NameChan ()
gameChannelEnd = do
    noName $ writeChan "Game cancelled."

--------------------------------------------------------------------------------
playName :: NameChan ()
playName = play (readNameMove, readNameMove) writeNameGame >>= writeNameResult

readNameMove :: Game -> NameChan Move
readNameMove game = do
    (mn, line) <- readChan
    case readMove game line of
        Right m -> return m
        Left e  -> writeChan (mn, "Error: " ++ e ++ ".") >> readNameMove game

writeNameGame :: Game -> NameChan ()
writeNameGame game = noName $ writeChanList (showGameLines game)

writeNameResult :: (Game,Result) -> NameChan ()
writeNameResult (_,Checkmate w)
  = noName $ writeChan (show w ++ " wins by checkmate.")
writeNameResult (_,Stalemate)
  = noName $ writeChan ("The game is drawn by stalemate.")

--------------------------------------------------------------------------------
type Name       = String
type NameChan a = Chan (Maybe Name, String) (Maybe Name, String) a

noName :: TextChan a -> NameChan a
noName = mapChan snd ((,) Nothing)

nameChan :: NameChan a -> TextChan a
nameChan = mapChan readName showName
  where
    readName :: String -> (Maybe Name, String)
    readName s = case break (=='>') s of
        ('<':h,'>':t) -> (Just h, if any isSpace (take 1 t) then drop 1 t else t)
        _             -> (Nothing, s)
    showName :: (Maybe Name, String) -> String
    showName (Just n,  s)  = n ++ ": " ++ s
    showName (Nothing, s) = s

--------------------------------------------------------------------------------
play :: Monad m
     => (Game -> m Move, Game -> m Move) -> (Game -> m ()) -> m (Game, Result)
play = play' initialGame

play' :: Monad m => Game
      -> (Game -> m Move, Game -> m Move) -> (Game -> m ()) -> m (Game, Result)
play' game (in1, in2) out = do
    out game
    move <- in1 game
    let game' = doMoveGame move game
    case result game' of
        Nothing -> play' game' (in2, in1) out
        Just r  -> out game' >> return (game', r)
