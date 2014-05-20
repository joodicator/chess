import Multiplex
import ChessControl
import ChessData
import ChessRules

main = runTextChan $ multiplex $ nameChan $ main'

main' :: NameChan ()
main' = gameChannel $ playName initialGame (nameHuman,nameHuman)
