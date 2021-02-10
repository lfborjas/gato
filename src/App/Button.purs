module App.Button where

import Data.Array (range)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Monoid ((<>))
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Prelude (Unit, bind, map, show, ($), (<$>))
import TicTacToe (GameState, Marker(..), blankBoard, markerAt, playAt)

type State = GameState 

data Action = MarkAt (Tuple Int Int)

component :: forall q i o m. (MonadEffect m) => H.Component HH.HTML q i o m
component =
  H.mkComponent
    { initialState: \_ ->  {currentTurn: X, board: blankBoard, winner: Nothing}
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

render :: forall cs m. State -> H.ComponentHTML Action cs m
render state =
  HH.div_
    [
      HH.text $ maybe "" (\w -> "The winner is" <> show w) state.winner,
      HH.table_ (renderRows)
    ]
  where
    renderRows = renderRow `map` range 0 2
    renderRow ix = HH.tr_ $ renderCell ix `map` range 0 2
    renderCell row col =
      HH.td 
        [HE.onClick \_ ->  markIfEmpty (Tuple row col)]
        [HH.text $ fromMaybe "" (show <$> markerAt (Tuple row col) state.board)]
    markIfEmpty pos =
      case (markerAt pos state.board) of
        Nothing -> Just $ MarkAt pos
        Just _ -> Nothing

handleAction :: forall cs o m. (MonadEffect m) => Action → H.HalogenM State Action cs o m Unit
handleAction = case _ of
  MarkAt pos -> do
    st <- H.get
    case (playAt pos st) of
      Left err -> H.liftEffect $ log $ "Error: " <> err
      Right {currentTurn, board, winner} -> 
        H.modify_ _ {currentTurn =  currentTurn, board = board , winner = winner}
