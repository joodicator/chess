--------------------------------------------------------------------------------
module ChessMinMaxAI where

import qualified Data.Set as S
import Data.Function
import Data.Maybe
import Data.List
import Control.Applicative
import Control.Monad
import Control.Monad.Trans.State.Strict

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

data SearchState = SearchState{
    searchDepth  :: !Int,
    nodeCount    :: !Int,
    leafDepthMin :: !Int,
    leafDepthMax :: !Int,
    leafDepthSum :: !Int,
    leafCount    :: !Int }

initialState :: SearchState
initialState = SearchState{
    searchDepth  = 0,
    nodeCount    = 0,
    leafDepthMin = maxBound,
    leafDepthMax = minBound, 
    leafDepthSum = 0,
    leafCount    = 0 }

--------------------------------------------------------------------------------
minMaxPlay :: Monad m => Int -> Game -> m Move
minMaxPlay l g = do
    Just m <- return $ minMaxPlay' l g
    return m

minMaxPlay' :: Int -> Game -> Maybe Move
minMaxPlay' l g@Game{gTurn=pc}
  = case minMaxPlay'' l g of
        ([], _,_) -> Nothing
        (vms,_,_) -> Just . snd $ maximumBy (compare `on` fst) vms

minMaxPlay'' :: Int -> Game -> ([(Value,Move)], Int, SearchState)
minMaxPlay'' l g@Game{gTurn=pc}
  = (vms,r,s)
  where
    ((vms,r),s) = runState (bestMove'' (Min,Max) l pc g es) initialState
    gs = catMaybes $ takeWhile isJust $ iterate (>>= undoMoveGame) (Just g)
    es = S.fromList $ map eqGame $ tail gs

--------------------------------------------------------------------------------
bestMove' :: (Value,Value)        -- (α,β) if c=pc else (β,α), for α-β pruning.
          -> Int                  -- Maximum remaining moves to examine.
          -> Colour               -- Player for whom value is represented.
          -> Game                 -- Current game state.
          -> S.Set EqGame         -- Previous game states.
          -> State SearchState    -- Algorithm variables and statistics.
             (Maybe (Value,Move), -- Move judged optimal for current player.
              Int)                -- Remaining move examination quota.
bestMove' p l c g@Game{gTurn=pc} es = do
    (vms, l') <- bestMove'' p l c g es
    case vms of
        _:_ -> return (Just $ maximumBy (comparePC `on` fst) vms, l')
        []  -> return (Nothing, l')
  where
    comparePC u v
      | c == pc   = compare u v
      | otherwise = compare v u

bestMove'' :: (Value,Value) -> Int -> Colour -> Game -> S.Set EqGame
           -> State SearchState ([(Value,Move)], Int)
bestMove'' p l c g@Game{gTurn=pc} es
  = let ms = legalMoves g in evaluate p l ms ([], 0)
  where
    comparePC :: Value -> Value -> Ordering
    comparePC u v
      | c == pc   = compare u v
      | otherwise = compare v u

    evaluate :: (Value,Value) -> Int -> [Move] -> ([(Value,Move)], Int)
             -> State SearchState ([(Value,Move)], Int)
    evaluate (best,worst) l ms@(m:ms') (vmsA,rA) = do
        let l'    = min l ((3*l) `div` (2*length ms))
        modify $ \s -> s{ searchDepth = searchDepth s + 1 }
        (v,r) <- evaluate' (worst,best) l' m
        modify $ \s -> s{ searchDepth = searchDepth s - 1 }
        let r'    = l - l' + r
            best' = maximumBy comparePC [best,v]
            p'    = (best',worst)
        case comparePC v worst of
            LT -> evaluate p' r' ms' ((v,m):vmsA, rA)
            _  -> return ((v,m):vmsA, r'+rA)
    evaluate _ l [] (vmsA,rA)
      = return (vmsA,l+rA)

    evaluate' :: (Value,Value) -> Int -> Move -> State SearchState (Value, Int)
    evaluate' p l m = do
        modify $ \s -> s{ nodeCount = 1 + nodeCount s }
        case v' of
            _ | not toCycle && l' > 0 -> do
                (mm,r) <- bestMove' p l' c g' (S.insert e es)
                return (maybe v' fst mm, r)
            _ | not toCycle -> do
                modify $ \s -> s{
                    leafCount    = 1 + leafCount s,
                    leafDepthSum = searchDepth s + leafDepthSum s,
                    leafDepthMin = min (searchDepth s) (leafDepthMin s),
                    leafDepthMax = max (searchDepth s) (leafDepthMax s)}
                return (v', l')
            Value v'' | not fromCycle ->
                return (Cycle v'', l')
            _ | otherwise ->
                return (v', l')
      where
        toCycle   = S.member (eqGame g') es
        fromCycle = S.member e es
        l'        = l - 1
        e         = eqGame g
        g'        = doMoveGame m g
        v'        = gameValue c g'

--------------------------------------------------------------------------------
gameValue :: Colour -> Game -> Value
gameValue c g = case result g of
    Just Checkmate{rWinner=wc} | wc==c  -> Win
    Just Stalemate                      -> Draw
    Nothing                             -> Value $ boardValue c g
    Just Checkmate{}                    -> Lose

boardValue :: Colour -> Game -> PieceValue
boardValue c game@Game{gBoard=board}
  = castleValue c game
  - max 0 (castleValue (oppose c) game - 1)
  + sum [pieceValue p | (_,(_,p)) <- B.list c board]
  - sum [max 0 (pieceValue p - 1) | (_,(_,p)) <- B.list (oppose c) board]

castleValue :: Colour -> Game -> PieceValue
castleValue c game@Game{gBoard=d, gMoves=ms}
  | length castles >= 2 = 6
  | length castles >= 1 = 3
  | otherwise           = 0
  where
    castles = do
        (k,(_,King)) <- B.list c d
        guard $ null $ pastMoves k ms
        (r,(_,Rook)) <- B.list c d
        guard $ null $ pastMoves r ms

pieceValue :: Piece -> PieceValue
pieceValue p = case p of
    King   -> 0
    Pawn   -> 5
    Knight -> 20
    Bishop -> 25
    Rook   -> 30
    Queen  -> 200

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
