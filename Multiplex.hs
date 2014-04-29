--------------------------------------------------------------------------------
module Multiplex(
    TextChan, runTextChan', runTextChan, multiplex, mapChan,
    Chan, readChan, writeChan, writeChanList, takeSubChan, feedSubChan,
    takeChan, feedChan, giveChan, feedChanList, giveChanList
) where

import Control.Applicative
import Data.List
import Data.Maybe
import Data.Char
import qualified Data.Map.Strict as M
import System.IO

--------------------------------------------------------------------------------
type TextChan a = Chan String String a

runTextChan :: TextChan a -> IO a
runTextChan = runTextChan' (stdin, stdout)

runTextChan' :: (Handle,Handle) -> TextChan a -> IO a
runTextChan' (inH, outH) chan = do
    hSetBuffering outH LineBuffering
    let (mx,lines,chan') = takeChan chan
    mapM_ (hPutStrLn outH) lines
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
takeChan (Chan ys e@(Left x))  = (Just x,  ys, Chan [] e)
takeChan (Chan ys e@(Right _)) = (Nothing, ys, Chan [] e)
