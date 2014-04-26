--------------------------------------------------------------------------------
module ChessText where

import Data.Char
import Data.List
import Data.Maybe
import Data.Tuple
import Data.Function
import Control.Applicative
import Control.Monad

import ChessData
import ChessBoard
import ChessRules

--------------------------------------------------------------------------------
type UserError    = String
type UserErrorE a = Either UserError a

--------------------------------------------------------------------------------
-- The given move in Standard Algebraic Notation.
showMove :: Board -> Move -> String
showMove d Move{mPath=t@(i,j), mCapture=mpc}
  = fromMaybe (showIndex i) prefix ++ maybe "" (const "x") mpc ++ showIndex j
  where
    prefix = fmap (\(c,p) -> showMovePrefix' d (c,p,t,mpc)) (d!i)
showMove d Passant{mPath=t}
  = showMove d Move{mPath=t, mCapture=Just Pawn} ++ "e.p."
showMove d Promote{mPath=t, mCapture=mpc, mPromote=pp}
  = showMove d Move{mPath=t, mCapture=mpc} ++ [showPiece pp]
showMove _ Castle{mKing=((_,fi),(_,fj))}
  | fj < fi   = "0-0-0"
  | otherwise = "0-0"

-- Pre: d!i==Just(c,p) ---------------------------------------------------------
-- The piece and source index part of the move in Standard Algebraic Notation.
showMovePrefix' :: Board -> (Colour,Piece,Path,Maybe Piece) -> String
showMovePrefix' d (c,p,t@(i,j),mpc)
  = (guard (p/=Pawn)                       >> [showPiece p]) ++
    (guard (any (on (==) file $ i) rivals) >> [showFile (file i)]) ++
    (guard (any (on (==) rank $ i) rivals) >> [showRank (rank i)])
  where
    rivals = [k | (k,(_,kp))<-list c d, kp==p, k/=i, couldMove' (c,kp,(k,j)) d]

--------------------------------------------------------------------------------
readMove :: Game -> String -> UserErrorE Move
readMove game s = do
    (spec,isC,isP) <- readMoveSpec game s
    move <- maybe (Left "illegal move") return (tryMove spec game)
    case (isP,isC,move) of
        (    _,     _, Passant{})                 -> return move
        (False,     _, Promote{mCapture=Just _})  -> return move
        (False,     _, Move{mCapture=Just _})     -> return move
        (False, False, _)                         -> return move
        _                                         -> Left "invalid move"

--------------------------------------------------------------------------------
type IsCapture = Bool
type IsPassant = Bool
type ExMoveSpec = (MoveSpec,IsCapture,IsPassant)

readMoveSpec :: Game -> String -> UserErrorE ExMoveSpec
readMoveSpec Game{gBoard=d, gTurn=pc} s
  = fromMaybe (Left "unrecognised syntax") (move s <|> castleK s <|> castleQ s)
  where
    move :: String -> Maybe (UserErrorE ExMoveSpec)
    move s = do
        (mip,s)        <- trimE (option $ char readPiece) s
        ((mi,j,isC),s) <- moveFrom s <|> moveTo (Nothing,Nothing) s
        (mpp,s)        <- trimS (option $ char readPiece) s
        (mIsP,s)       <- trimS (option $ word "e.p.") s
        (_,[])         <- space s
        return (move' (mi,j,fromMaybe Pawn mip,mpp,isC,isJust mIsP))
    moveFrom :: String
             -> Maybe (((Maybe Rank,Maybe File),Index,IsCapture), String)
    moveFrom s = do
        (mfi,s) <- option (char readFileIndex) s
        (mri,s) <- option (char readRankIndex) s
        (_,s)   <- space s
        moveTo (mri,mfi) s
    moveTo :: (Maybe Rank,Maybe File) -> String
           -> Maybe (((Maybe Rank,Maybe File),Index,IsCapture), String)
    moveTo mi s = do
        (mIsC,s) <- option (word "x") s
        (_,s)  <- space s
        (j,s)  <- readIndex s
        return ((mi,j,isJust mIsC),s)
    move' :: ((Maybe Rank,Maybe File),Index,Piece,Maybe Piece,IsCapture,IsPassant)
          -> (UserErrorE ExMoveSpec)
    move' ((mri,mfi),j,ip,mpp,isC,isP) = do
        let is = do
            (i@(ri,fi),(_,ip')) <- list pc d; guard (ip'==ip)
            maybe (return ()) (guard . (==) ri) mri
            maybe (return ()) (guard . (==) fi) mfi
            guard (couldMove' (pc,ip,(i,j)) d)
            return i
        case is of
            []  -> Left "illegal move"
            [i] -> return (((i,j),mpp),isC,isP)
            _   -> Left "ambiguous move"

    castleK :: String -> Maybe (UserErrorE ExMoveSpec)
    castleK s = do
        (_,s)  <- word "0-0" s
        (_,[]) <- space s
        return (castle 2)
    castleQ :: String -> Maybe (UserErrorE ExMoveSpec)
    castleQ s = do
        (_,s)  <- word "0-0-0" s;
        (_,[]) <- space s
        return (castle (-2))
    castle :: File -> UserErrorE ExMoveSpec
    castle fd = do
        let kings = indices d (pc,King)
        (rk,fk) <- maybe (Left "invalid move") return (listToMaybe kings)
        return ((((rk,fk),(rk,fk+fd)),Nothing),False,False)

    char :: (Char -> Maybe a) -> String -> Maybe (a, String)
    char f s = do c:s' <- return s; x <- f c; return (x, s')
    
    word :: String -> String -> Maybe ((), String)
    word w s = do s' <- stripPrefix w s; return ((), s')
    
    option :: (String -> Maybe (a, String)) -> String -> Maybe (Maybe a, String)
    option f s = Just $ maybe (Nothing,s) (\(x,s') -> (Just x,s')) (f s)
    
    trimS :: (String -> Maybe (a, String)) -> String -> Maybe (a, String)
    trimS f s = f (dropWhile isSpace s)
    
    trimE :: (String -> Maybe (a, String)) -> String -> Maybe (a, String)
    trimE f s = (\(x,s) -> (x,dropWhile isSpace s)) <$> f s
    
    space :: String -> Maybe ((), String)
    space s = Just ((), dropWhile isSpace s)

--------------------------------------------------------------------------------
showIndex :: Index -> String
showIndex (r,f)
  = showFile f : showRank r : []

readIndex :: String -> Maybe (Index, String)
readIndex cs = do
    fc:rc:cs' <- return cs
    f <- readFileIndex fc
    r <- readRankIndex rc
    return ((r,f),cs')

--------------------------------------------------------------------------------
rankChars :: [(Rank,Char)]
rankChars = zip ranks ['1'..]

fileChars :: [(File,Char)]
fileChars = zip files ['a'..]

showRank :: Rank -> Char
showRank = fromJust . flip lookup rankChars

showFile :: File -> Char
showFile = fromJust . flip lookup fileChars

readRankIndex :: Char -> Maybe Rank
readRankIndex = flip lookup (map swap rankChars)

readFileIndex :: Char -> Maybe File
readFileIndex = flip lookup (map swap fileChars)

--------------------------------------------------------------------------------
showPiece :: Piece -> Char
showPiece p = case p of
    Pawn    -> 'P'
    Rook    -> 'R'
    Knight  -> 'N'
    Bishop  -> 'L'
    Queen   -> 'Q'
    King    -> 'K'

readPiece :: Char -> Maybe Piece
readPiece c = case toUpper c of
    'P' -> Just Pawn
    'R' -> Just Rook
    'N' -> Just Knight
    'L' -> Just Bishop
    'Q' -> Just Queen
    'K' -> Just King
    _   -> Nothing

--------------------------------------------------------------------------------
showEmptySquare :: Index -> Char
showEmptySquare p
  = case squareColour p of
      Black -> '+'
      White -> '.'

showColourPiece :: (Colour,Piece) -> Char
showColourPiece (c,p)
  = case c of
      Black -> toUpper (showPiece p)
      White -> toLower (showPiece p)

showSquare :: Index -> Maybe (Colour,Piece) -> Char
showSquare i = maybe (showEmptySquare i) showColourPiece

readSquare :: Char -> Maybe (Colour,Piece)
readSquare c
  = fmap readSquare' (readPiece c)
  where
    readSquare' p
      | isUpper c = (Black,p)
      | otherwise = (White,p)

--------------------------------------------------------------------------------
readBoard :: String -> Board
readBoard = readBoardLines . lines

readBoardLines :: [String] -> Board
readBoardLines lines = fromList $ do
    (r,line) <- zip (reverse ranks) (drop 1 lines)
    (f,char) <- zip files (filter (/= ' ') (drop 1 line))
    return ((r,f), readSquare char)

--------------------------------------------------------------------------------
showBoard :: Board -> String
showBoard = unlines . showBoardLines

printBoard :: Board -> IO ()
printBoard = mapM_ putStrLn . showBoardLines

showBoardLines :: Board -> [String]
showBoardLines b
  = map (intersperse ' ') $ fileRow : map rankRow (reverse ranks) ++ [fileRow]
  where
    rankRow r  = [showRank r] ++ rankRow' r ++ [showRank r]
    rankRow' r = [showSquare (r,f) (b ! (r,f)) | f <- files]
    fileRow    = ' ' : map showFile files

showLegendLines :: Int -> [String]
showLegendLines n
   = joinColumns $ map (map pieceLegend) $ divide n $ [minBound::Piece ..]
   where
     pieceLegend p = showColourPiece (White,p) : showColourPiece (Black,p)
                   : ' ' : map toLower (show p)

--------------------------------------------------------------------------------
joinColumns :: [[String]] -> [String]
joinColumns = joinColumns' "  "

joinColumns' :: String -> [[String]] -> [String]
joinColumns' sep cols
  = map (dropWhileEnd (== ' ') . concat . intersperse sep) padRows
  where
    padRows = map (zipWith (pad ' ') widths) rows
    rows    = transpose padCols
    padCols = map (pad "" height) cols
    height  = maximum (map length cols)
    widths  = map (maximum . map length) cols
    pad x n = take n . (++ repeat x)

divide :: Int -> [a] -> [[a]]
divide n _ | n<1 = error "divide with non-positive length"
divide _ []      = []
divide n xs      = let (hs,ts) = splitAt n xs in hs : divide n ts
