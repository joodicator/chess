module ChessBoard(
    Board,          Update,     (!),        (//),
    empty,          get,        put,        list,
    listAll,        fromList,   update,     indices
) where

import qualified Data.Map.Strict as M
import Data.List
import Control.Applicative hiding (empty)

import ChessData

type Map    = M.Map Index (Colour,Piece)
type Update = (Index,Maybe (Colour,Piece))
data Board  = Board{ white::Map, black::Map } deriving (Eq, Ord)

instance Show Board where show = const "Board"

(!) = get
(//) = update

empty :: Board
empty = Board{white=M.empty, black=M.empty}

get :: Board -> Index -> Maybe (Colour,Piece)
get Board{white=w, black=b} i
  = M.lookup i w <|> M.lookup i b

put :: Board -> Index -> Maybe (Colour,Piece) -> Board
put d@Board{white=w, black=b} i e
  = case e of
      Just e@(White,_) -> d{white=M.insert i e w, black=M.delete i   b}
      Just e@(Black,_) -> d{white=M.delete i   w, black=M.insert i e b}
      Nothing          -> d{white=M.delete i   w, black=M.delete i   b}

list :: Colour -> Board -> [(Index,(Colour,Piece))]
list White Board{white=w} = M.toList w
list Black Board{black=b} = M.toList b

listAll :: Board -> [(Index,(Colour,Piece))]
listAll Board{white=w, black=b}
  = M.toList w ++ M.toList b

fromList :: [(Index,Maybe (Colour,Piece))] -> Board
fromList = update empty

update :: Board -> [(Index,Maybe (Colour,Piece))] -> Board
update = foldl' (uncurry . put)

indices :: Board -> (Colour,Piece) -> [Index]
indices d (c,p) = [i | (i,(_,p')) <- list c d, p==p']
