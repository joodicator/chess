--------------------------------------------------------------------------------
module ChessText where

import Data.Char
import Data.List
import Data.Maybe
import Data.Tuple
import Data.Function
import Control.Monad

import ChessData
import ChessBoard
import ChessRules

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
    rivals = do
        (k,(kc,kp)) <- list c d; guard (kp==p && k/=i)
        when (isNothing mpc) $ guard (couldJustMove' (kc,kp,(k,j)) d)
        when (isJust mpc)    $ guard (couldCapture'  (kc,kp,(k,j)) d)
        return k

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
      Black -> ','
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
    (r,line) <- zip (reverse ranks) lines
    (f,char) <- zip files (filter (/= ' ') line)
    return ((r,f), readSquare char)

--------------------------------------------------------------------------------
showBoard :: Board -> String
showBoard = unlines . showBoardLines

printBoard :: Board -> IO ()
printBoard = mapM_ putStrLn . showBoardLines

showBoardLines :: Board -> [String]
showBoardLines b
  = map (intersperse ' ') $ map rankRow (reverse ranks) ++ [lastRow]
  where
    rankRow r = [showSquare (r,f) (b ! (r,f)) | f <- files] ++ [showRank r]
    lastRow = map showFile files

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
