{-# LANGUAGE CPP #-}

module Standard where

#if __GLASGOW_HASKELL__ < 700

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = reverse . dropWhile p . reverse

instance Monad (Either e) where
    return        = Right
    Left  x >>= f = Left x
    Right y >>= f = f y

#endif
