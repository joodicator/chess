--------------------------------------------------------------------------------
module Output(
    Output(..), Out, OColour(..), plain, bold, colour,
    TextChan, runTextChan', runTextChan, multiplex, mapChan,
    Chan, readChan, writeChan, writeChanList, takeSubChan, feedSubChan,
    takeChan, feedChan, giveChan, feedChanList, giveChanList
) where

import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe
import Data.Char
import qualified Data.Map as M
import System.IO

--------------------------------------------------------------------------------
data Output
  = PlainText | ANSITerminal | IRCMessage
  deriving Show

type Out a = Output -> a

plain :: a -> Out a
plain = const

bold :: String -> Out String
bold s o = case o of
    ANSITerminal -> "\27[1m" ++ s ++ "\27[21m"
    IRCMessage   -> "\2" ++ s ++ "\2"
    _            -> s

data OColour
  = LRed | LYellow | LGreen | LCyan | LBlue | LMagenta | LGrey | OWhite
  | DRed | DYellow | DGreen | DCyan | DBlue | DMagenta | DGrey | OBlack
  deriving (Show, Enum, Bounded)

colour :: (OColour, Maybe OColour) -> String -> Out String
colour (f,Just b)  s IRCMessage
  = "\3" ++ ircCC' f ++ "," ++ ircCC' b ++ s ++ "\3"
colour (f,Nothing) s IRCMessage
  = "\3" ++ ircCC' f ++ s ++ "\3"
colour (f,Just b)  s ANSITerminal
  = "\27[" ++ show (ansiCC f) ++ ";"
           ++ show (ansiCC b + 10) ++ "m" ++ s ++ "\27[39;49m"
colour (f,Nothing) s ANSITerminal
  = "\27[" ++ show (ansiCC f)  ++ "m" ++ s ++ "\27[39m"
colour _ s _
  = s

ircCC' :: OColour -> String
ircCC' = reverse . take 2 . (++ repeat '0') . reverse . show . ircCC

ircCC :: OColour -> Int
ircCC c = case c of
    LRed     -> 04;    LYellow  -> 08;    LGreen   -> 09;    LCyan    -> 14;
    DRed     -> 05;    DYellow  -> 07;    DGreen   -> 03;    DCyan    -> 10;
    LBlue    -> 12;    LMagenta -> 13;    LGrey    -> 15;    OWhite   -> 00;
    DBlue    -> 02;    DMagenta -> 06;    DGrey    -> 14;    OBlack   -> 01

ansiCC :: OColour -> Int
ansiCC c = case c of
    LRed     -> 91;    LYellow  -> 93;    LGreen   -> 92;    LCyan    -> 96;
    DRed     -> 31;    DYellow  -> 33;    DGreen   -> 32;    DCyan    -> 36;
    LBlue    -> 94;    LMagenta -> 95;    LGrey    -> 37;    OWhite   -> 97;
    DBlue    -> 34;    DMagenta -> 35;    DGrey    -> 90;    OBlack   -> 30

--------------------------------------------------------------------------------
type TextChan a = Chan String String a

runTextChan :: TextChan a -> IO a
runTextChan = runTextChan' (stdin, stdout)

runTextChan' :: (Handle,Handle) -> TextChan a -> IO a
runTextChan' (inH, outH) chan = do
    hSetBuffering outH LineBuffering
    let (mx,lines,chan') = takeChan chan
    mapM (hPutStrLn outH) lines
    case mx of
      Nothing -> do
        line <- hGetLine inH
        let (_,chan'') = giveChan line chan'
        runTextChan' (inH,outH) chan''
      Just x -> do
        return x

multiplex :: TextChan a -> TextChan ()
multiplex = multiplex' M.empty

multiplex' :: M.Map String (TextChan a) -> TextChan a -> TextChan ()
multiplex' chans proto = do
    (name,line) <- chanName <$> readChan
    let chan = M.findWithDefault proto name chans
    let (reply,_,chan') = feedChan line chan
    writeChanList (map (name ++) reply)
    let (mx,_,_) = takeChan chan'
    let chans' = maybe (M.insert name chan') (const $ M.delete name) mx $ chans
    multiplex' chans' proto

chanName :: String -> (String, String)
chanName s@('#':_) = (h ++ take 1 t, drop 1 t) where (h,t) = break isSpace s
chanName s         = ("", s)

mapChan :: (i' -> i) -> (o -> o') -> Chan i o a -> Chan i' o' a
mapChan iMap oMap chan = do
    let (mx,lines,chan') = takeChan chan
    writeChanList (map oMap lines)
    case mx of
      Nothing -> do
        item <- fmap iMap readChan
        let (_,chan'') = giveChan item chan'
        mapChan iMap oMap chan''
      Just x -> do
        return x

--------------------------------------------------------------------------------
data Chan i o a = Chan [o] (Either a (i -> Chan i o a))

instance Show (Chan i o a) where
    show _ = "Chan"

instance Monad (Chan i o) where
    return = chanPure
    (>>=)  = chanBind

instance Applicative (Chan i o) where
    pure      = return
    af <*> ax = af >>= \f -> ax >>= \x -> return (f x)

instance Functor (Chan i o) where
    fmap f c = c >>= (return . f)

chanPure :: a -> Chan i o a
chanPure x = Chan [] (Left x)

chanBind :: Chan i o a -> (a -> Chan i o b) -> Chan i o b
chanBind (Chan ys e) f
  = Chan (ys ++ ys') e'
  where
    Chan ys' e' = either f (\g -> Chan [] (Right $ (>>= f) . g)) e

--------------------------------------------------------------------------------
readChan :: Chan i o i
readChan = Chan [] (Right chanPure)

writeChan :: o -> Chan i o ()
writeChan x = writeChanList [x]

writeChanList :: [o] -> Chan i o ()
writeChanList xs = Chan xs (Left ())

feedSubChan :: i -> Chan i o a -> Chan i o (Chan i o a)
feedSubChan z c = do
    let (ys,_,c') = feedChan z c
    writeChanList ys
    return c'

takeSubChan :: Chan i o a -> Chan i o (Chan i o a)
takeSubChan c = do
    let (_,ys,c') = takeChan c
    writeChanList ys
    return c'

--------------------------------------------------------------------------------
feedChan :: i -> Chan i o a -> ([o], Maybe i, Chan i o a)
feedChan z c = (ys, mz, c'') where
    (mz, c')     = giveChan z c
    (_, ys, c'') = takeChan c'

feedChanList :: [i] -> Chan i o a -> ([o], [i], Chan i o a)
feedChanList zs c = (ys, zs', c'') where
    (zs', c')    = giveChanList zs c
    (_, ys, c'') = takeChan c'

giveChan :: i -> Chan i o a -> (Maybe i, Chan i o a)
giveChan z c = (listToMaybe zs, c')
  where (zs, c') = giveChanList [z] c

giveChanList :: [i] -> Chan i o a -> ([i], Chan i o a)
giveChanList (z:zs) (Chan ys (Right g))
  = (zs', writeChanList ys >> c')
  where (zs', c') = giveChanList zs (g z)
giveChanList zs c = (zs, c)

takeChan :: Chan i o a -> (Maybe a, [o], Chan i o a)
takeChan (Chan ys e)
  = (either Just (const Nothing) e, ys, Chan [] e)
