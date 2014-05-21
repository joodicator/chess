import Control.Monad
import Data.List

import ChessData
import ChessText
import ChessRules
import ChessControl
import Multiplex
import ChessMinMaxAI

strength = 4

main = runNameChan $ playName initialGame (nameHuman, minMaxPlay strength)
