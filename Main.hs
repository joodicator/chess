import Control.Monad
import Control.Applicative
import Data.List
import Data.Function
import Data.Char
import Data.Ord
import Data.Maybe
import Numeric
import System.IO
import System.Environment
import System.Exit

import Standard
import Output
import ChessData
import ChessText
import ChessRules
import ChessControl
import ChessMinMaxAI

defaultDepthLimit = 4 :: Int

main = do
    depthLimit <- getDepthLimit
    players <- getPlayers depthLimit
    runNameChan $ playName initialGame players ANSITerminal

aiPlayer :: Int -> Game -> NameChan Move
aiPlayer depthLimit game@Game{gBoard=d} = do
    let (vms,s) = minMaxPlay'' depthLimit game
    vms'@((_,(_,m):_):_) <- return $ reverse $ sortBy (compare `on` fst) vms
    let SearchState{sNodeCount=nodeCount} = s
    noName . writeChan $ unwords ["n=" ++ show nodeCount, "m=" ++ show depthLimit]
    noName . writeChanList $ do
        (v,ms) <- vms'
        let sms = do
            (g,m) <- ms
            return $ showMove (gBoard g) m
        return $ showValue v ++ ": " ++ intercalate ", " sms
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
    
humanPlayer :: Int -> Game -> NameChan Move
humanPlayer depthLimit game = do
    (mn, line) <- readChan
    case map toLower line of
        "ai" -> aiPlayer depthLimit game
        _    -> id =<< feedSubChan (mn, line) (nameHuman game)

getPlayers :: Int -> IO (Game -> NameChan Move, Game -> NameChan Move)
getPlayers depthLimit = do
    args <- filter (not . isPrefixOf "--") <$> getArgs
    case args of
        [white, black] ->
            case (getPlayer depthLimit white, getPlayer depthLimit black) of
                (Just whiteP, Just blackP) -> return (whiteP, blackP)
                _                          -> exitUsage
        [] -> return (humanPlayer depthLimit, aiPlayer depthLimit)
        _  -> exitUsage

getPlayer :: Int -> String -> Maybe (Game -> NameChan Move)
getPlayer depthLimit spec = case map toLower spec of
    s@(_:_) | s `isPrefixOf` "human" -> Just $ humanPlayer depthLimit
    s@(_:_) | s `isPrefixOf` "ai"    -> Just $ aiPlayer depthLimit
    _                                -> Nothing

getDepthLimit :: IO Int
getDepthLimit = do
    args <- getArgs
    return . fromMaybe defaultDepthLimit . listToMaybe $ do
        arg <- reverse args
        Just arg' <- return $ stripPrefix "--depth=" arg
        return (read arg')

exitUsage :: IO a
exitUsage = do
    progName <- getProgName
    hPutStrLn stderr $ "Usage: " ++ progName ++ " [(ai|human) (ai|human)]"
    exitFailure
