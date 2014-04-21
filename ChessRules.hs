--------------------------------------------------------------------------------
module ChessRules where

import Control.Monad
import Control.Applicative
import Data.Maybe

import ChessData
import ChessBoard
import ChessText

--------------------------------------------------------------------------------
initialBoard :: Board
initialBoard = readBoardLines [
    "R N L Q K L N R 8",
    "P P P P P P P P 7",
    ". , . , . , . , 6",
    ", . , . , . , . 5",
    ". , . , . , . , 4",
    ", . , . , . , . 3",
    "p p p p p p p p 2",
    "r n l q k l n r 1",
    "a b c d e f g h  "]

initialGame :: Game
initialGame = Game{gBoard=initialBoard, gTurn=White, gMoves=[]}

--------------------------------------------------------------------------------
doMove :: Move -> Board -> Board
doMove m d = d // doMove' m d

doMove' :: Move -> Board -> [Update]
doMove' m board = case m of
    Move{ mPath=t }                     -> path t
    Passant{ mPath=t@(i,j) }            -> ((rank i,file j),Nothing) : path t
    Promote{ mPath=(i,j), mPromote=p }  -> (i,Nothing) : (j,promote i p) : []
    Castle{ mKing=kt, mRook=rt }        -> path kt ++ path rt
  where
    path (i,j)  = (i,Nothing) : (j,board!i) : []
    promote i p = (\(c,_) -> (c,p)) <$> board!i

--------------------------------------------------------------------------------
undoMove :: Move -> Board -> Board
undoMove m d = d // undoMove' m d

undoMove' :: Move -> Board -> [Update]
undoMove' m board = case m of
    Move{ mPath=(i,j), mCapture=mp }    -> (j,free i mp) : (i,board!j) : []
    Promote{ mPath=(i,j), mCapture=mp } -> (j,free i mp) : (i,demote j) : []
    Passant{ mPath=t@(i@(r,_),(_,f)) }  -> ((r,f),free i (Just Pawn)) : unpath t
    Castle{ mKing=kt, mRook=rt }        -> unpath kt ++ unpath rt
  where
    unpath (i,j) = (j,Nothing) : (i,board!j) : []
    demote i     = (\(c,_) -> (c,Pawn)) <$> board!i
    free i mp    = (\(c,_) p -> (oppose c,p)) <$> board!i <*> mp

--------------------------------------------------------------------------------
inCheck :: Colour -> Board -> Bool
inCheck pc board = False

--------------------------------------------------------------------------------
-- Succeeds with the move specified by (t,mpp) iff it is legal in this game.
tryMove :: (Path, Maybe Promotion) -> Game -> Maybe Move
tryMove (t@(i,j),mpp) game@Game{gBoard=board, gTurn=pc} = do
    (ic,ip) <- board!i
    guard (ic == pc)
    move <- tryPromote' (ip,mpp) game =<< tryMove' (ip,t) game
    guard (not $ inCheck pc $ doMove move board)
    return move

-- Pre: board!i==Just(pc,ip) where (i,j)=t -------------------------------------
-- Gives a legal move, ignoring pawn promotion and check.
tryMove' :: (Piece, Path) -> Game -> Maybe Move
tryMove' (ip,t) g@Game{gBoard=board, gTurn=pc} = case ip of
    Pawn | (rd `div` up) `elem` [1,2] && fd==0 ->
        trySlide' t g >> tryJustMove' t g
    Pawn | rd==up && abs fd==1 ->
        tryJustCapture' t g <|> tryPassant' t g
    Rook | (rd==0) /= (fd==0) ->
        trySlide' t g >> tryMaybeCapture' t g
    Knight | abs rd+abs fd==3 && rd/=0 && fd/=0 ->
        tryMaybeCapture' t g
    Bishop | abs rd==abs fd && rd /= 0 ->
        trySlide' t g >> tryMaybeCapture' t g
    Queen | (rd==fd || rd==0 || fd==0) && (rd,fd)/=(0,0) ->
        trySlide' t g >> tryMaybeCapture' t g
    King | max (abs rd) (abs fd) == 1 ->
        tryJustMove' t g <|> tryCastle' t g
    _ -> Nothing
  where
    ((R ri,F fi),(R rj,F fj)) = t
    (rd, fd) = (rj-ri, fj-fi)
    up = case pc of White -> 1; Black -> -1    

-- Succeeds iff the squares strictly between i and j are all empty. ------------
trySlide' :: Path -> Game -> Maybe ()
trySlide' (i@(ri,fi), j@(rj,fj)) Game{gBoard=board}
  = mapM_ (guard . isNothing . (board !)) path
  where
    (rd,fd) = (signum (rj-ri), signum (fj-fi))
    path = takeWhile (/=j) $ drop 1 $ iterate (\(r,f) -> (r+rd, f+fd)) i

-- Jumps from i to j, if j is empty. -------------------------------------------
tryJustMove' :: Path -> Game -> Maybe Move
tryJustMove' t@(i,j) Game{gBoard=d, gTurn=pc} = case d!j of
    Nothing -> Just Move{mPath=t, mCapture=Nothing}
    _       -> Nothing

-- Jumps from i to j, if j holds an opponent's piece. --------------------------
tryJustCapture' :: Path -> Game -> Maybe Move
tryJustCapture' t@(_,j) Game{gBoard=d, gTurn=pc} = case d!j of
    Just (jc,jp) | jc/=pc -> Just Move{mPath=t, mCapture=Just jp}
    _                     -> Nothing

-- Jumps from i to j, if it is empty or holds an opponent's piece. -------------
tryMaybeCapture' :: Path -> Game -> Maybe Move
tryMaybeCapture' t g
  = tryJustMove' t g <|> tryJustCapture' t g

-- Pre: board!i==(pc,Pawn) where Game{gBoard=board,gTurn=pc}=game --------------
-- Performs an /en passant/ capture, if legal (ignoring check).
tryPassant' :: Path -> Game -> Maybe Move
tryPassant' (i,j) game = Nothing

-- Pre: board!i==(pc,King) where Game{gBoard=board,gTurn=pc}=game --------------
-- Performs a castling move, if legal (ignoring check)
tryCastle' :: Path -> Game -> Maybe Move
tryCastle' (i,j) game = Nothing


-- Pre: (gBoard g)!i==(pc,ip) where (i,j)=mPath move ---------------------------
-- Given a legal move ignoring pawn promotion and check, produces
-- a legal move obeying pawn promotion and ignoring check.
tryPromote' :: (Piece, Maybe Promotion) -> Game -> Move -> Maybe Move
tryPromote' (ip,mpp) g@Game{gTurn=pc} move = case (ip,mpp,move) of
    (Pawn, Nothing, Move{mPath=t}) -> do
        guard (rank (snd t) /= lastRank)
        return move
    (Pawn, Just pp, Move{mPath=t, mCapture=mpc}) -> do
        guard (rank (snd t) == lastRank)
        return Promote{mPath=t, mCapture=mpc, mPromote=pp}
    (_, Nothing, _) -> Just move
    (_, Just _, _)  -> Nothing
  where
    lastRank = case pc of White -> last ranks; Black -> head ranks
