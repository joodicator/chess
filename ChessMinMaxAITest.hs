import Control.Monad
import Data.List
import Data.Function
import Data.Char
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

l = 10^5

main = do
    players <- getPlayers
    runNameChan $ playName initialGame players ANSITerminal

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

humanPlayer = nameHuman

aiPlayer game@Game{gBoard=d} = do
    let (vms,r,s) = minMaxPlay'' l game
    vms'@((_,m):_) <- return $ reverse $ sortBy (compare `on` fst) vms
    let SearchState{nodeCount=nC, leafCount=lC, leafDepthSum=dS,
                    leafDepthMin=dm, leafDepthMax=dM} = s
    let ws = ["nC=" ++ show nC ++ " (" ++ show (100 * nC `div` l) ++ "%)",
              "lC=" ++ show lC]
    let ws' | lC == 0   = []
            | otherwise = ["dm=" ++ show dm,
                           "da=" ++ showFFloatAlt (Just 1)
                             ((fromIntegral $ dS) / (fromIntegral lC)) "",
                           "dM=" ++ show dM]
    noName . writeChan . intercalate ", " $ ws ++ ws'
    noName . writeChan . unwords $ do
        (v,m) <- vms'
        return $ "(" ++ show v ++ "," ++ showMove d m ++ ")"
    return m

