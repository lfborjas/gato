module TicTacToe where

import Prelude

import Control.Alternative ((<|>))
import Data.Array (catMaybes, findMap, fromFoldable, head, index, length, nub, range, replicate, updateAt, (!!))
import Data.Either (Either(..), note)
import Data.List (transpose)
import Data.List as L
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..))

data Marker = X | O

derive instance eqMarker :: Eq Marker
derive instance ordMarker :: Ord Marker

instance showMarker :: Show Marker where
  show X = "X"
  show O = "O"

type Tile = Maybe Marker
type Position = Tuple Int Int

type Board = Array (Array Tile)

type GameState = {
  currentTurn :: Marker,
  board :: Board,
  winner :: Maybe Marker
}

blankBoard :: Board
blankBoard =
  rows
  where
    row = replicate 3 Nothing
    rows = replicate 3 row

next :: Marker -> Marker
next X = O
next O = X

markerAt :: Position -> Board -> Maybe Marker
markerAt (Tuple rowPos colPos) board = do
  row <- index board rowPos
  fromMaybe Nothing $ index row colPos

playAt :: Position -> GameState -> Either String GameState
playAt pos@(Tuple rowPos colPos) gs@{currentTurn, board, winner} = do
  alreadyWon <- maybe (pure gs) (const <<< Left $ "game over!") winner
  occupied <- maybe (pure gs) (const <<< Left $ "Invalid move") $ markerAt pos board
  row <- note "Invalid row" $ board !! rowPos
  updatedRow <- note "Invalid column" $ updateAt colPos (Just nextTurn) row
  updatedBoard <- note "Invalid row" $ updateAt rowPos updatedRow board
  pure $ {currentTurn: nextTurn, board: updatedBoard, winner: findWinner updatedBoard}
  where
    nextTurn = next currentTurn

winsRow :: Array Tile -> Maybe Marker
winsRow r = 
  if length occupied == 3 && length (nub occupied) == 1 then
    head occupied
  else
    Nothing
  where
    occupied = catMaybes r


findWinner :: Board -> Maybe Marker
findWinner board =
  findMap winsRow rows <|> findMap winsRow cols <|> findMap winsRow diagonals
  where
    rows = board
    -- this is of course very wasteful, indicative that they should be lists!
    cols = fromFoldable $ map fromFoldable $ transpose (L.fromFoldable $ map  L.fromFoldable board)
    diagonals = 
      [map (\i -> markerAt (Tuple i  i) board) (range 0 2)
      , map (\i -> markerAt (Tuple i (3-i)) board) (range 1 3)]
