module ChessBoard(
    Board,          Update,     (!),        (//),
    empty,          get,        put,        list,
    listAll,        fromList,   update,     indices
) where

import qualified Data.Map as M
import Data.List
import ChessData

type Map = M.Map Index Piece
type Update = (Index,Maybe (Colour,Piece))
data Board = Board{ white::Map, black::Map } deriving (Eq, Ord)

instance Show Board where show = const "Board"

(!) = get
(//) = update

empty :: Board
empty = Board{white=M.empty, black=M.empty}

get :: Board -> Index -> Maybe (Colour,Piece)
get Board{white=w, black=b} i
  = case (M.lookup i w, M.lookup i b) of
      (Just p, _) -> Just (White,p)
      (_, Just p) -> Just (Black,p)
      _           -> Nothing

put :: Board -> Index -> Maybe (Colour,Piece) -> Board
put d@Board{white=w, black=b} i e
  = case e of
      Just (White,p) -> d{white=M.insert i p w, black=M.delete i   b}
      Just (Black,p) -> d{white=M.delete i   w, black=M.insert i p b}
      Nothing        -> d{white=M.delete i   w, black=M.delete i   b}

list :: Colour -> Board -> [(Index,(Colour,Piece))]
list White Board{white=w} = [(i,(White,p)) | (i,p) <- M.toList w]
list Black Board{black=b} = [(i,(Black,p)) | (i,p) <- M.toList b]

listAll :: Board -> [(Index,(Colour,Piece))]
listAll d = list White d ++ list Black d

fromList :: [(Index,Maybe (Colour,Piece))] -> Board
fromList = update empty

update :: Board -> [(Index,Maybe (Colour,Piece))] -> Board
update = foldl' (uncurry . put)

indices :: Board -> (Colour,Piece) -> [Index]
indices d (c,p) = [i | (i,(_,p')) <- list c d, p==p']