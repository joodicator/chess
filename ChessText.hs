--------------------------------------------------------------------------------
module ChessText where

import Data.Char
import Data.List
import Data.Maybe

import ChessData
import ChessBoard(Board)
import qualified ChessBoard as B

--------------------------------------------------------------------------------
showRank :: Rank -> Char
showRank (R n) = chr ((ord '1') + n - 1)

showFile :: File -> Char
showFile (F n) = chr ((ord 'a') + n - 1)

instance Show Rank where show = return . showRank
instance Show File where show = return . showFile

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
readBoardLines lines = B.fromList $ do
    (r,line) <- zip (reverse ranks) lines
    (f,char) <- zip files (filter (/= ' ') line)
    return ((r,f), readSquare char)

--------------------------------------------------------------------------------
showBoard :: Board -> String
showBoard = unlines . showBoardLines

showBoardLines :: Board -> [String]
showBoardLines b
  = map (intersperse ' ') $ map rankRow (reverse ranks) ++ [lastRow]
  where
    rankRow r = [showSquare (r,f) (b B.! (r,f)) | f <- files] ++ [showRank r]
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
