--------------------------------------------------------------------------------
module ChessText where

import Data.Char
import Data.List
import Data.Maybe
import Data.Tuple
import Data.Function
import Control.Applicative
import Control.Monad

import Standard
import ChessData
import ChessBoard
import ChessRules
import Output

--------------------------------------------------------------------------------
type UserError    = String
type UserErrorE a = Either UserError a

--------------------------------------------------------------------------------
-- The given move in Standard Algebraic Notation.
showMove :: Board -> Move -> String
showMove = showMove' False

showMoveLong :: Board -> Move -> String
showMoveLong = showMove' True

showMove' :: Bool -> Board -> Move -> String
showMove' long d Move{mPath=t@(i,j), mCapture=mpc}
  = piece ++ fromMaybe (showIndex i) (guard (not long) >> from) ++ to
  where
    piece = maybe "" (\(_,p) -> if p==Pawn then "" else [showPiece p]) (d!i)
    from  = fmap (\(c,p) -> showMovePrefix' d (c,p,t,mpc)) (d!i)
    to    = maybe "" (const "x") mpc ++ showIndex j
showMove' long d Passant{mPath=t}
  = showMove' long d Move{mPath=t, mCapture=Just Pawn} ++ "e.p."
showMove' long d Promote{mPath=t, mCapture=mpc, mPromote=pp}
  = showMove' long d Move{mPath=t, mCapture=mpc} ++ [showPiece pp]
showMove' _ _ Castle{mKing=((_,fi),(_,fj))}
  | fj < fi   = "0-0-0"
  | otherwise = "0-0"

-- Pre: d!i==Just(c,p) ---------------------------------------------------------
-- The piece and source index part of the move in Standard Algebraic Notation.
showMovePrefix' :: Board -> (Colour,Piece,Path,Maybe Piece) -> String
showMovePrefix' d (c,p,t@(i,j),mpc)
  = (guard (any (on (==) file $ i) rivals) >> [showFile (file i)]) ++
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
        return (move' (mi,j,mip,mpp,isC,isJust mIsP))
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
    move' :: ((Maybe Rank,Maybe File),Index,Maybe Piece,Maybe Piece,
              IsCapture,IsPassant) -> (UserErrorE ExMoveSpec)
    move' ((mri,mfi),j,mip,mpp,isC,isP) = do
        mip <- return $ case (mri,mfi,mip) of
            (Just _,Just _,     _) -> mip
            (     _,     _,Just _) -> mip
            _                      -> Just Pawn
        let is = do
            (i@(ri,fi),(_,ip)) <- list pc d;
            maybe (return ()) (guard . (==) ip) mip
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
pieceMap = [
    ('P',Pawn),
    ('R',Rook),
    ('N',Knight),
    ('B',Bishop),
    ('Q',Queen),
    ('K',King)]

showPiece :: Piece -> Char
showPiece p = fromJust $ lookup p (map swap pieceMap)

readPiece :: Char -> Maybe Piece
readPiece c = lookup c pieceMap

--------------------------------------------------------------------------------
showEmptySquare :: Index -> Char
showEmptySquare = showEmptySquare' . squareColour

showEmptySquare' :: Colour -> Char
showEmptySquare' Black = ' '
showEmptySquare' White = ' '

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
    (f,char) <- zip files (filter (not . (`elem` "[ ]")) line)
    return ((r,f), readSquare char)

--------------------------------------------------------------------------------
showGame :: Game -> Out String
showGame = (fmap unlines) . showGameLines

printGame :: Game -> IO ()
printGame = mapM_ putStrLn . ($ ANSITerminal) . showGameLines

showBoard :: Board -> Out String
showBoard = (fmap unlines) . showBoardLines

printBoard :: Board -> IO ()
printBoard = mapM_ putStrLn . ($ ANSITerminal) . showBoardLines

showGameLines :: Game -> Out [String]
showGameLines game@Game{gBoard=board, gTurn=pc, gMoves=moves}
  = joinColumns columns
  where
    columns o
      = [boardLines, topLines ++ legendLines ++ bottomLines]
      where
        boardLines   = showBoardLines' marked board o
        legendLines  = replicate legendHeight ""
        legendHeight = length boardLines - length topLines - length bottomLines
        marked = case moves of
            Move    { mPath=(_,j) } : _              -> [j]
            Promote { mPath=(_,j) } : _              -> [j]
            Castle  { mKing=(_,k), mRook=(_,j) } : _ -> [k,j]
            Passant { mPath=(_,j) } : _              -> [j]
            []                                       -> []
    topLines       = [noteLine Black, captureLine Black]
    bottomLines    = [captureLine White, noteLine White]
    noteLine c     = show c ++ noteLine' c
    noteLine' c    = if c==pc then playerLine else opponentLine
    captureLine c  = case filter (\(c',_) -> c' /= c) (capturedPieces game) of
        [] -> ""
        cs -> "Captured: " ++ reverse (map showColourPiece cs)
    playerLine = case (canMove game, inCheck game) of
        (True, False)  -> " to play."
        (True, True)   -> " to play (in check)."
        (False, True)  -> ": checkmate."
        (False, False) -> ": stalemate."
    opponentLine = case moves of
        m:_ -> ": " ++ showMoveLong (undoMoveBoard m board) m
        []  -> ""

showBoardLines :: Board -> Out [String]
showBoardLines = showBoardLines' []

showBoardLines' :: [Index] -> Board -> Out [String]
showBoardLines' marked d o
  = map rankRow (reverse ranks)
  where
    edge       = colour (DGrey,Nothing)
    rankRow r  = concat ["[" ++ c ++ "]" | c <- rankRow' r]
    rankRow' r = do
        f <- files
        let mcp = (d ! (r,f))
        return $! if (r,f) `elem` marked
            then colour (LRed,Nothing) [showSquare (r,f) mcp] o
            else showSquareOut (r,f) mcp o

showSquareOut :: Index -> Maybe (Colour,Piece) -> Out String
showSquareOut i mcp
  = style [showSquare i mcp]
  where
    style = case mcp of
        Nothing -> colour (DGrey,Nothing)
        Just _  -> plain

--------------------------------------------------------------------------------
joinColumns :: Out [[String]] -> Out [String]
joinColumns = joinColumns' "  "

joinColumns' :: String -> Out [[String]] -> Out [String]
joinColumns' sep cols o
  = map (dropWhileEnd (== ' ') . concat . intersperse sep) padRows
  where
    padRows = map (zipWith padZipped pWidths) rows
    pWidths = map (maximum . map (length . snd)) zipCols
    rows    = transpose padCols
    padCols = map (pad ("","") height) zipCols
    height  = maximum (map length zipCols)
    zipCols = zipWith zip (cols o) (cols PlainText)
    pad x n = take n . (++ repeat x)
    padZipped n (os,ps) = take (n + length os - length ps) (os ++ repeat ' ')

divide :: Int -> [a] -> [[a]]
divide n _ | n<1 = error "divide with non-positive length"
divide _ []      = []
divide n xs      = let (hs,ts) = splitAt n xs in hs : divide n ts

