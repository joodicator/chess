import Control.Monad
import Data.List
import Data.Function
import Data.Char
import Data.Ord
import Numeric
import System.IO
import System.Environment
import System.Exit

import ChessData
import ChessText
import ChessRules
import ChessControl
import Output
import ChessMinMaxAI

depthLimit = 6

main = do
    players <- getPlayers
    runNameChan $ playName initialGame players ANSITerminal

aiPlayer :: Game -> NameChan Move
aiPlayer game@Game{gBoard=d} = do
    let (vms,s) = minMaxPlay'' depthLimit game
    vms'@((_,m):_) <- return $ reverse $ sortBy (compare `on` fst) vms
    let SearchState{sNodeCount=nodeCount} = s
    let ws = ["n=" ++ show nodeCount, "m=" ++ show depthLimit]
    noName . writeChan . unwords $ ws ++ do
        (v,m) <- vms'
        return $ "(" ++ showValue v ++ "," ++ showMove d m ++ ")"
    return m
 where
    showValue v = case v of
        Min            -> "Min"
        Max            -> "Max"
        Win (Down n) v -> "Win("  ++ show n ++ "):" ++ show v
        Lose  v dn     -> "Lose:"  ++ showValue (Value v dn)
        Draw  v dn     -> "Draw:"  ++ showValue (Value v dn)
        Cycle v dn     -> "Cycle:" ++ showValue (Value v dn)
        Value v (Down n)
            | n == depthLimit -> show v
            | otherwise       -> show v ++ "(" ++ show n ++ ")"
    
humanPlayer :: Game -> NameChan Move
humanPlayer = nameHuman

getPlayers :: IO (Game -> NameChan Move, Game -> NameChan Move)
getPlayers = do
    args <- getArgs
    case args of
        [white, black] ->
            case (getPlayer white, getPlayer black) of
                (Just whiteP, Just blackP) -> return (whiteP, blackP)
                _                          -> exitUsage
        [] -> return (humanPlayer, aiPlayer)
        _  -> exitUsage

exitUsage :: IO a
exitUsage = do
    progName <- getProgName
    hPutStrLn stderr $ "Usage: " ++ progName ++ " [(ai|human) (ai|human)]"
    exitFailure

getPlayer :: String -> Maybe (Game -> NameChan Move)
getPlayer spec = case map toLower spec of
    "human" -> Just humanPlayer
    "ai"    -> Just aiPlayer
    _       -> Nothing
