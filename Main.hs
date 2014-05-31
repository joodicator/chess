import System.Environment
import Control.Applicative

import Output
import ChessControl
import ChessData
import ChessRules

main = main' =<< getOutput

main' :: Out (IO ())
main' = runTextChan . multiplex . nameChan . main''

main'' :: Out (NameChan ())
main'' = gameChannel . playName initialGame (nameHuman,nameHuman)

defaultOutput :: Output
defaultOutput = ANSITerminal

getOutput :: IO Output
getOutput
  = getOutput' . reverse <$> getArgs
  where
    getOutput' ("--plain":_) = PlainText
    getOutput' ("--ansi":_)  = ANSITerminal
    getOutput' ("--irc":_)   = IRCMessage
    getOutput' (_:ss)        = getOutput' ss
    getOutput' []            = defaultOutput

