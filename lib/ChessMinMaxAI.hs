--------------------------------------------------------------------------------
module ChessMinMaxAI where

import qualified Data.Set as S
import Data.Function
import Data.Maybe
import Data.List
import Control.Applicative
import Control.Monad

import qualified ChessBoard as B
import ChessData
import ChessRules

--------------------------------------------------------------------------------
type PieceValue = Int

data Value
  = Min | Lose | Draw | Cycle Value | Value PieceValue | Win | Max
  deriving (Eq, Ord)

instance Show Value where
    show Min       = "Min"
    show Lose      = "Lose"
    show Draw      = "Draw"
    show (Cycle v) = "Cycle " ++ show v
    show (Value v) = show v
    show Win       = "Win"
    show Max       = "Max"

data EqGame = EqGame{
    eBoard   :: Board,
    eTurn    :: Colour,
    ePassant :: Maybe Index,
    eCastle  :: S.Set Index}
    deriving (Eq, Ord)

--------------------------------------------------------------------------------
minMaxPlay :: Monad m => Int -> Game -> m Move
minMaxPlay l g = do
    (_,m):_ <- return $ minMaxPlay' l g
    return m

minMaxPlay' :: Int -> Game -> [(Value,Move)]
minMaxPlay' l g@Game{gTurn=pc}
  = bestMoves l pc g es
  where
    gs = catMaybes $ takeWhile isJust $ iterate (>>= undoMoveGame) (Just g)
    es = S.fromList $ map eqGame gs

bestMoves :: Int -> Colour -> Game -> S.Set EqGame -> [(Value,Move)]
bestMoves l c g@Game{gTurn=pc} es
  = sortBy order $ map evaluate $ legalMoves g
  where
    order (u,_) (v,_)
      | c == pc   = compare v u
      | otherwise = compare u v
    evaluate m = case ms of
      _ | S.member e' es -> (Cycle v', m)
      _ | l <= 1         -> (v', m)
      []                 -> (v', m)
      (v,_):_            -> (v, m)
      where
        ms = bestMoves (l-1) c g' (S.insert e' es)
        v' = gameValue c g'
        g' = doMoveGame m g
        e' = eqGame g'

--------------------------------------------------------------------------------
gameValue :: Colour -> Game -> Value
gameValue c g@Game{gBoard=board} = case result g of
    Just Checkmate{rWinner=wc} | wc==c  -> Win
    Just Stalemate                      -> Draw
    Nothing                             -> Value $ boardValue c board
    Just Checkmate{}                    -> Lose

boardValue :: Colour -> Board -> PieceValue
boardValue c board
  = sum [pieceValue p | (_,(_,p)) <- B.list c board]
  - sum [pieceValue p | (_,(_,p)) <- B.list (oppose c) board]

pieceValue :: Piece -> PieceValue
pieceValue King   = 0
pieceValue Pawn   = 1
pieceValue Rook   = 5
pieceValue Knight = 5
pieceValue Bishop = 5
pieceValue Queen  = 20

--------------------------------------------------------------------------------
eqGame :: Game -> EqGame
eqGame Game{gBoard=board, gTurn=pc, gMoves=ms}
  = EqGame{eBoard=board, eTurn=pc, ePassant=mpi, eCastle=sci}
  where
    mpi = do
        Move{mPath=(i@(ri,fi),j@(rj,fj))} : _ <- return ms
        (_,Pawn) <- board B.! i
        guard (fj==fi && abs(rj-ri)==2)
        return j
    sci = S.fromList $ do
        (k,(c,King)) <- B.listAll board
        guard (null $ pastMoves k ms)
        (i,(_,Rook)) <- B.list c board
        guard (null $ pastMoves i ms)
        return i
