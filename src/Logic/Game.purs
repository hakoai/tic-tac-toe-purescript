module Logic.Game 
  ( Koma(..)
  , GameState(..)
  , Error(..)
  , Location
  , G
  , update
  , mkState
  , getList
  , getState
  )
where

import Prelude

import Data.Either (Either, note)
import Data.List (List, mapWithIndex, reverse, tail, transpose, (:))
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (sequence)
import Data.Unfoldable (replicate)

data Koma = Brank | Maru | Batsu

data GameState = MaruTurn | BatsuTurn | MaruWin | BatsuWin | Drow

data Error = NotGame | NotPut

data Location = Location Int Int

newtype G = G
  { count :: Int
  , ban :: Ban
  , state :: GameState
  }

update :: Location -> G -> Either Error G
update (Location x y) (G {state: state, ban: ban, count: count}) = do
  koma <- note NotGame $ getKoma state
  newBan <- note NotPut $ put koma x y ban
  let isWin = checkWin count koma newBan
  pure $ if isWin
    then   G { state: (getWin koma),      ban: newBan, count: count }
    else if checkWinMaybe count Maru newBan || checkWinMaybe count Batsu newBan
      then G { state: (turnChange state), ban: newBan, count: count }
      else G { state: Drow,               ban: newBan, count: count } 

mkState :: Int -> Int -> G
mkState n nn = G {state: MaruTurn, ban: mkBan n, count: nn }

getList :: forall value. (Location -> Koma -> value) -> G -> List (List value)
getList func (G {ban: (Ban l)}) = mapWithIndex (mapWithIndex <<< ((\fn y x -> fn (Location x y)) func)) l

getState :: G -> GameState
getState (G {state: r}) = r


newtype Ban = Ban (List (List Koma))

mkBan :: Int -> Ban
mkBan n = Ban $ replicate n $ replicate n Brank

rotate :: forall t10. List (List t10) -> List (List t10)
rotate l = reverse $ transpose l

checkWin :: Int -> Koma -> Ban -> Boolean
checkWin count koma (Ban ban) =
     checkAll (_ == koma) count ban
  || checkAll (_ == koma) count (rotate ban)

checkWinMaybe :: Int -> Koma -> Ban -> Boolean
checkWinMaybe count koma (Ban ban) =
     checkAll (\k -> k == koma || k == Brank) count ban
  || checkAll (\k -> k == koma || k == Brank) count (rotate ban)

checkRight :: forall koma. (koma -> Boolean) -> Int -> List koma -> Boolean
checkRight koma 0 _ = true
checkRight koma n (x:lx) | koma x = checkRight koma (n-1) lx
checkRight _ _ _ = false

mkRightList :: forall a. List (List a) -> Maybe (List (List a))
mkRightList l = sequence $ tail <$> l

mkDouwnRightList :: forall a. List (List a) -> Maybe (List (List a))
mkDouwnRightList (_:lx) = mkRightList lx
mkDouwnRightList _ = Nothing

checkDownRight :: forall koma. (koma -> Boolean) -> Int -> List (List koma) -> Boolean
checkDownRight koma 0 _ = true
checkDownRight koma n l@((x:_):_) | koma x =
  maybe false (checkDownRight koma (n-1)) $ mkDouwnRightList l
checkDownRight _ _ _ = false

checkDown :: forall koma. (koma -> Boolean) -> Int -> List (List koma) -> Boolean
checkDown koma 0 _ = true
checkDown koma n ((x:_):lx) | koma x = checkDown koma (n-1) lx
checkDown _ _ _ = false

checkDownAll :: forall koma. (koma -> Boolean) -> Int -> List (List koma) -> Boolean
checkDownAll koma count l@(ll@(x:_):lx) | koma x =
     checkDown koma count l 
  || checkDownRight koma count l
  || checkRight koma count ll
  || checkDownAll koma count lx
checkDownAll koma count ((_:_):lx) = checkDownAll koma count lx
checkDownAll _ _ _ = false

checkAll :: forall koma. (koma -> Boolean) -> Int -> List (List koma) -> Boolean
checkAll koma count l =
     checkDownAll koma count l
  || maybe false (checkAll koma count) (mkRightList l)

putRight :: Koma -> Int -> List Koma -> Maybe (List Koma)
putRight koma 0 (Brank:lx) = Just $ koma:lx
putRight _ 0 _ = Nothing
putRight koma n (x:lx) = do
  nlx <- putRight koma (n -1) lx
  pure $ x:nlx
putRight _ _ _ = Nothing

putDoun :: Koma -> Int -> Int -> List (List Koma) -> Maybe (List (List Koma))
putDoun koma xn 0 (x:lx) = do
  nx <- putRight koma xn x
  pure $ nx:lx
putDoun _ _ 0 _ = Nothing
putDoun koma xn n (x:lx) = do
  nlx <- putDoun koma xn (n - 1) lx
  pure $ x:nlx
putDoun _ _ _ _ = Nothing

put :: Koma -> Int -> Int -> Ban -> Maybe Ban
put koma x y (Ban l) = map Ban $ putDoun koma x y l

getKoma :: GameState -> Maybe Koma
getKoma MaruTurn = Just Maru
getKoma BatsuTurn = Just Batsu
getKoma _ = Nothing

turnChange :: GameState -> GameState
turnChange MaruTurn = BatsuTurn
turnChange BatsuTurn = MaruTurn
turnChange a = a

getWin :: Koma -> GameState
getWin Maru = MaruWin
getWin Batsu = BatsuWin
getWin _ = Drow

derive instance eqKoma :: Eq Koma
