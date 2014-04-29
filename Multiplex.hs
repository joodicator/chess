--------------------------------------------------------------------------------
module Multiplex(
    Chan, readChan, writeChan, writeChanList,
    takeChan, feedChan, giveChan, feedChanList, giveChanList
) where

import Data.Maybe

--------------------------------------------------------------------------------
data Chan i o a = Chan [o] (Either a (i -> Chan i o a))

instance Show (Chan i o a) where
    show _ = "Chan"

instance Monad (Chan i o) where
    return = chanPure
    (>>=)  = chanBind

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
