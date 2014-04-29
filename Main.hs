import Multiplex
import ChessControl

main = runTextChan $ multiplex $ nameChan $ gameChannel
