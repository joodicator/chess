import Control.Monad
import Data.List
import Data.Function

import ChessData
import ChessText
import ChessRules
import ChessControl
import Multiplex
import ChessMinMaxAI

l = 10^5

main = runNameChan $ playName initialGame (aiPlay, aiPlay)

aiPlay game@Game{gBoard=d} = do
    let (vms,r) = minMaxPlay'' l game
    vms'@((_,m):_) <- return $ reverse $ sortBy (compare `on` fst) vms
    noName . writeChan . unwords $ ["n=" ++ show (l-r)] ++ do
        (v,m) <- vms'
        return $ "(" ++ show v ++ "," ++ showMove d m ++ ")"
    return m

