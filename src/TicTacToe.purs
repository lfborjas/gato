module TicTacToe where

import Prelude

import Control.Alternative ((<|>))
import Data.Array (catMaybes, concat, filter, findMap, fromFoldable, head, index, length, nub, null, range, replicate, updateAt, (!!))
import Data.Either (Either(..), note)
import Data.List (transpose)
import Data.List as L
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
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

data GameOver 
  = Tied
  | Victory Marker

instance showGameOver :: Show GameOver where
  show Tied  = "It's a tie!"
  show (Victory m) = "The winner is: " <> show m

type GameState = {
  currentTurn :: Marker,
  board :: Board,
  outcome :: Maybe GameOver
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
playAt pos@(Tuple rowPos colPos) gs@{currentTurn, board, outcome} = do
  gameOver <- maybe (pure unit) (const <<< Left $ "Game over!") outcome
  occupied <- maybe (pure unit) (const <<< Left $ "Invalid move") $ markerAt pos board
  row <- note "Invalid row" $ board !! rowPos
  updatedRow <- note "Invalid column" $ updateAt colPos (Just currentTurn) row
  updatedBoard <- note "Invalid row" $ updateAt rowPos updatedRow board
  pure $ {currentTurn: nextTurn, board: updatedBoard, outcome: determineOutcome updatedBoard}
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

determineOutcome :: Board -> Maybe GameOver
determineOutcome board = 
  if isTie board then
    Just Tied
  else
    Victory <$> findWinner board 

findWinner :: Board -> Maybe Marker
findWinner board =
  findMap winsRow rows <|> findMap winsRow cols <|> findMap winsRow diagonals
  where
    rows = board
    -- FIXME(luis) this is of course very wasteful, indicative that maybe I
    -- want to hand-write it, or use Lists throughout (Array weirdly has more
    -- convenience functions readily available?)
    cols = fromFoldable $ map fromFoldable $ transpose (L.fromFoldable $ map  L.fromFoldable board)
    diagonals = 
      [map (\i -> markerAt (Tuple i  i) board) (range 0 2)
      , map (\i -> markerAt (Tuple (i-1) (3-i)) board) (range 1 3)]

isTie :: Board -> Boolean
isTie = 
  concat >>> filter isNothing >>> null
