module View.Game where
  
import Prelude

import Data.Either (Either(..))
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Logic.Game (G, GameState(..), Koma(..), Location, getList, getState, mkState, update)

type State = 
  { g:: G
  , ticOrGo:: String
  }

data Query a
  = Tap Location a
  | Reset a
  | ChangeBan String a

type Input = Unit

toStateString :: GameState -> String
toStateString MaruTurn = "〇のターンです"
toStateString BatsuTurn = "×のターンです"
toStateString MaruWin = "〇の勝ちです"
toStateString BatsuWin = "×の勝ちです"
toStateString Drow = "引き分けです"

ticTacTor :: String
ticTacTor = "三目並べ"
gomoku :: String
gomoku = "五目並べ"

comp :: forall m. H.Component HH.HTML Query Unit Void m
comp =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing }

render :: State -> H.ComponentHTML Query
render state =
  let 
    turn = getState state.g
    ban = L.toUnfoldable $ L.toUnfoldable <$> getList (\l k ->(Tuple l k)) state.g
  in
    HH.div_
      [ HH.div
        [ HP.classes
          [ H.ClassName "uk-text-center"
          ]
        ]
        [ HH.text $ toStateString turn
        ]
        , renderBan ban
        , HH.div
          [ HH.attr (HH.AttrName "style") """
            width: 300px;
            margin: auto;
          """
          ]
          [ HH.button
            [ HP.classes
              [ H.ClassName "uk-button"
              , H.ClassName "uk-button-default"
              ]
              , HE.onClick (HE.input_ (Reset))
            ]
            [ HH.text "Reset"]
          , HH.select
            [ HP.classes
              [ H.ClassName "uk-select"
              ]
              , HE.onValueChange (HE.input (ChangeBan))
            ]
            [ HH.option_ [HH.text ticTacTor]
            , HH.option_ [HH.text gomoku]
            ]
          ]
      ]

renderBan :: Array (Array (Tuple Location Koma)) -> H.ComponentHTML Query
renderBan = HH.div
  [HH.attr (HH.AttrName "style") """
          width: 300px;
          margin: auto;
        """
] <<< map (\l ->
  HH.div
    [ HH.attr (HH.AttrName "uk-grig") ""
    , HP.classes
      [ H.ClassName "uk-grid-collapse"
      , H.ClassName "uk-grid"
      , H.ClassName "uk-child-width-expand"
      ]
    ,  HH.attr (HH.AttrName "style") """
        """
    ] $
    map (\(Tuple location koma) ->
      HH.div [
        HP.classes
          [ H.ClassName "cell-line"
          ]
      ]
      [
      HH.div
        [ HE.onClick (HE.input_ (Tap location))
        , HP.classes
          [ H.ClassName "uk-card"
          , H.ClassName "uk-card-default"
          , H.ClassName "uk-text-center"
          ]
        ,  HH.attr (HH.AttrName "style") """
          position: absolute;
          top: 0;
          width: 100%;
          height: 100%;
        """
        ]
        [ HH.div
            [
              HH.attr (HH.AttrName "style") """
                position: absolute;
                top: 50%;
                left: 50%;
                transform: translateY(-50%) translateX(-50%);
                -webkit- transform: translateY(-50%) translateX(-50%);
                font-size: 18px;
        """
            ]
            [ HH.text (if koma == Maru then "〇" else if koma == Batsu then "×" else "")] ]
      ]
    ) l
)

ticTacTorState :: G
ticTacTorState = mkState 3 3
gomokuState :: G
gomokuState = mkState 15 5

initialState :: { g :: G
, ticOrGo :: String
}
initialState = {g:ticTacTorState, ticOrGo: ticTacTor}

eval :: forall message m. Query ~> H.ComponentDSL State Query message m
eval = case _ of
  Tap location next -> do
    state <- H.get
    let nextState = update location state.g
    case nextState of
      Right g -> do
        H.put (state {g=g})
        pure next
      _ -> pure next
  Reset next -> do
    state <- H.get
    if state.ticOrGo == ticTacTor
      then do
        H.put {g: ticTacTorState, ticOrGo: ticTacTor}
      else if state.ticOrGo == gomoku
        then H.put {g: gomokuState, ticOrGo: gomoku}
        else pure unit
    pure next
  ChangeBan v next -> do
    if v == ticTacTor
      then do
        H.put {g: ticTacTorState, ticOrGo: ticTacTor}
      else if v == gomoku
        then H.put {g: gomokuState, ticOrGo: gomoku}
        else pure unit
    pure next
