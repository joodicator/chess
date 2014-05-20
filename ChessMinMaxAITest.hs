import Control.Monad
import Data.List

import ChessData
import ChessText
import ChessRules
import ChessControl
import Multiplex
import ChessMinMaxAI

strength = 4

main = runNameChan $ playName initialGame (nameHuman, nameAI)

nameAI :: Game -> NameChan Move
nameAI game = do
    vms@((_,m):_) <- return $ minMaxPlay' strength game
    noName . writeChan . unwords $ do
        (v,m) <- vms
        return $ "(" ++ show v ++ "," ++ showMove (gBoard game) m ++ ")"
    return m

