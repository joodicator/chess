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
  = Min | Lose | Draw | Cycle PieceValue | Value PieceValue | Win | Max
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
    Just m <- return $ minMaxPlay' l g
    return m

minMaxPlay' :: Int -> Game -> Maybe Move
minMaxPlay' l g@Game{gTurn=pc}
  = snd <$> bestMove l pc g es
  where
    gs = catMaybes $ takeWhile isJust $ iterate (>>= undoMoveGame) (Just g)
    es = S.fromList $ map eqGame $ tail gs

--------------------------------------------------------------------------------
bestMove
    :: Int          -- Maximum depth of recursion.
    -> Colour       -- Player (not necessarily current) whose value is used.
    -> Game         -- Current game state.
    -> S.Set EqGame -- All previous game states.
    -> Maybe (Value,Move)
bestMove = bestMove' (Min,Max)

bestMove'
    :: (Value,Value) -> Int -> Colour -> Game -> S.Set EqGame
    -> Maybe (Value,Move)
bestMove' (best,worst) l c g@Game{gTurn=pc} es
  = case evaluate best (legalMoves g) of
        []  -> Nothing
        vms -> Just $ maximumBy (comparePC `on` fst) vms
  where
    comparePC :: Value -> Value -> Ordering
    comparePC u v
      | c == pc   = compare u v
      | otherwise = compare v u

    evaluate :: Value -> [Move] -> [(Value,Move)]
    evaluate best' (m:ms)
      | comparePC v worst == LT = (v,m) : evaluate best'' ms
      where
        best'' = maximumBy comparePC [best', v] 
        v      = evaluate' m (worst, best'')
    evaluate _ _ = []

    evaluate' :: Move -> (Value,Value) -> Value
    evaluate' m p = case v' of
        Value v'' | toCycle && not fromCycle -> Cycle v''
        _         | toCycle                  -> v'
        _         | l <= 1                   -> v'
        _         | otherwise                -> maybe v' fst mm
      where
        toCycle   = S.member (eqGame g') es
        fromCycle = S.member e es
        mm = bestMove' p (l-1) c g' (S.insert e es)
        e  = eqGame g
        g' = doMoveGame m g
        v' = gameValue c g'

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
