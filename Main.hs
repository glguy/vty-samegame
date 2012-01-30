module Main where

import Data.IORef
import Data.Array
import Data.Maybe
import Control.Concurrent
import Control.Exception
import Control.Monad
import Graphics.Vty
import System.Random
import qualified Data.Set as Set
import Data.Set (Set)

scoringFunction i = (i - 2) ^ (2 :: Int)
clearScreenBonus  = 1000
animationDelay    = 150000
gameSize          = (9,14)

mkNewGame :: IO GameState
mkNewGame = do
  arr <- newBoard gameSize
  return (mkGameState (0,0) arr Nothing 0)

main :: IO ()
main = bracket mkVty shutdown $ \vty -> do
  eventVar <- newMVar Refresh
  gameVar  <- newIORef =<< mkNewGame 
  forkIO (clock eventVar)
  forkIO (drawingThread vty eventVar gameVar 0)
  
  let loop = do
        ev <- next_event vty
        case ev of
          EvKey KUp      [] -> transform (moveCursor coordUp)
          EvKey KDown    [] -> transform (moveCursor coordDown)
          EvKey KLeft    [] -> transform (moveCursor coordLeft)
          EvKey KRight   [] -> transform (moveCursor coordRight)
          EvKey KEnter   [] -> transform captureGroup
          EvKey KEsc     [] -> return ()
          EvKey (KASCII 'n') [] -> restart
          _                 -> loop

      restart = do
        g <- mkNewGame
        transform (\_ -> g)

      transform f = do
        modifyIORef gameVar f
        putMVar eventVar Refresh
        loop
  loop


-- * Game board

type Coord = (Int,Int)

data GameState = GameState
  { cursorCoord        :: Coord
  , boardArray         :: Array Coord (Maybe Piece)
  , cachedConnectedSet :: Set Coord
  , lastScore          :: Maybe Int
  , totalScore         :: Int
  , gameOver           :: Bool
  }

mkGameState ::
  Coord {- ^ current cursor coordinate -}         ->
  Array Coord (Maybe Piece) {- ^ current board -} ->
  Maybe Int {- ^ recent score change -}           ->
  Int {- ^ total score -}                         ->
  GameState
mkGameState c a ls ts = GameState
  { cursorCoord        = c
  , boardArray         = a
  , cachedConnectedSet = connectedSet c a
  , lastScore          = ls
  , totalScore         = ts
  , gameOver           = isGameOver a
  }

coordUp, coordDown, coordLeft, coordRight :: Coord -> Coord
coordUp    (r,c) = (r-1,c)
coordDown  (r,c) = (r+1,c)
coordLeft  (r,c) = (r,c-1)
coordRight (r,c) = (r,c+1)

moveCursor :: (Coord -> Coord) -> GameState -> GameState
moveCursor direction st
  | rangeTest cursor' = mkGameState cursor' (boardArray st) Nothing (totalScore st)
  | otherwise         = st
  where
  cursor'   = direction (cursorCoord st)
  rangeTest = inArray (boardArray st)

captureGroup :: GameState -> GameState
captureGroup st
  | groupSize < 2 = st
  | otherwise = mkGameState (cursorCoord st) board' (Just score) (totalScore st + score)
  where
  groupSize = Set.size (cachedConnectedSet st)
  board' = collapseHoles (deleteSet (cachedConnectedSet st) (boardArray st))
  score = scoringFunction groupSize + bonus

  bonus
    | all isNothing (elems board') = clearScreenBonus
    | otherwise                    = 0

isGameOver :: Array Coord (Maybe Piece) -> Bool
isGameOver a = not (any isPaired (assocs a))
  where
  isPaired (_,Nothing) = False
  isPaired (c,Just p)  = any isMatch (neighbors a c)
    where
    isMatch x = a ! x == Just p

deleteSet :: Set Coord -> Array Coord (Maybe Piece) -> Array Coord (Maybe Piece)
deleteSet set arr = arr // [ (c, Nothing) | c <- Set.toList set ]


collapseHoles :: Array Coord (Maybe Piece) -> Array Coord (Maybe Piece)
collapseHoles arr = accumArray (\_ a -> Just a) Nothing (bounds arr)
                               (toCoords arr (arrayToList arr))

arrayToList :: Array Coord (Maybe Piece) -> [[Piece]]
arrayToList arr =
  filter (not . null)     $
  flip map      [colLo..colHi]         $ \c ->
  flip mapMaybe [rowHi,rowHi-1..rowLo] $ \r ->
    arr ! (r,c)
  where
  ((rowLo, colLo),(rowHi,colHi)) = bounds arr

toCoords arr = concat . zipWith (\c -> zipWith (\r x -> ((r,c),x)) [rowHi,rowHi-1..rowLo]) [0..]
  where
  ((rowLo, colLo),(rowHi,colHi)) = bounds arr

newBoard :: Coord -> IO (Array Coord (Maybe Piece))
newBoard sz = do
  let n = (fst sz + 1) * (snd sz + 1)
  xs <- replicateM n randomPiece
  return (listArray ((0,0),sz) (map Just xs))

connectedSet :: Coord -> Array Coord (Maybe Piece) -> Set Coord
connectedSet c arr = case arr ! c of
  Nothing -> Set.empty
  Just p -> aux (Set.singleton c) Set.empty
    where
    aux q res = case Set.minView q of
      Nothing -> res
      Just (x,q')
        | arr ! x /= Just p  -> aux q' res
        | Set.member x res   -> aux q' res
        | otherwise          -> aux (foldr Set.insert q' (neighbors arr x)) (Set.insert x res)

neighbors :: Array Coord (Maybe Piece) -> Coord -> [Coord]
neighbors a (x,y) = filter (inArray a) [(x+1,y),(x-1,y),(x,y-1),(x,y+1)]

-- | Return 'True' if the given index is in the given array.
inArray :: Array Coord e -> Coord -> Bool
inArray arr = inRange (bounds arr)

-- * Drawing functions

activeChar :: Int -> Char
activeChar 0 = '⬖'
activeChar 1 = '◆'
activeChar 2 = '⬗'
activeChar 3 = '◇'

animationLength :: Int
animationLength = 4

inactiveChar :: Char
inactiveChar = '◆'

emptyChar :: Char
emptyChar = ' '


data RedrawEvent = TimerTick | Refresh

data Piece = Red | Purple | Green
  deriving (Eq)

randomPiece :: IO Piece
randomPiece = do
  x <- randomRIO (0, 2)
  return $! case x :: Int of
    0 -> Red
    1 -> Purple
    2 -> Green

pieceToAttr :: Piece -> Attr
pieceToAttr Red    = with_fore_color def_attr red
pieceToAttr Green  = with_fore_color def_attr green
pieceToAttr Purple = with_fore_color def_attr magenta

clock :: MVar RedrawEvent -> IO ()
clock eventVar = forever $ do
  threadDelay animationDelay
  putMVar eventVar TimerTick

toCursor :: Coord -> Cursor
toCursor (r,c) = Cursor (fromIntegral c + 1) (fromIntegral r + 1)

drawingThread :: Vty -> MVar RedrawEvent -> IORef GameState -> Int -> IO ()
drawingThread vty eventVar gameVar ctr = do
  ev <- takeMVar eventVar
  st <- readIORef gameVar
  let ctr' = (ctr + 1) `mod` animationLength
  case ev of
    TimerTick -> update vty (draw ctr' st) >> drawingThread vty eventVar gameVar ctr'
    Refresh   -> update vty (draw ctr st) >> drawingThread vty eventVar gameVar ctr

draw ctr st = Picture { pic_image = image
                      , pic_cursor = toCursor (cursorCoord st)
                      , pic_background = Background ' ' def_attr }
  where
  image = boxImage boardImage <|> char def_attr ' ' <|> infoImage

  infoImage = string def_attr "══╡ CONSOLE GAME ╞══"
          <-> char def_attr ' '
          <-> string def_attr "Score: "
          <|> string (with_fore_color def_attr red) (show (totalScore st))
          <-> char def_attr ' '
          <-> lastScoreImage
          <-> char def_attr ' '
          <-> gameOverImage
          <-> char def_attr ' '
          <-> controlsImage

  controlsImage = string def_attr "Controls: ←↑↓→ ␛ ⏎"
              <-> string def_attr "New game: N"

  lastScoreImage = case lastScore st of
    Nothing -> char def_attr ' '
    Just n  -> string def_attr ("Group worth " ++ show n)

  gameOverImage
    | gameOver st = string def_attr "Game Over!"
    | otherwise   = char def_attr ' '

  activeSet = cachedConnectedSet st

  ((rowLo, colLo),(rowHi,colHi)) = bounds (boardArray st)
  boardImage = vert_cat  [drawRow r                            | r <- [rowLo..rowHi]]
  drawRow r  = horiz_cat [drawCell r c (boardArray st ! (r,c)) | c <- [colLo..colHi]]

  drawCell _ _ Nothing      = char def_attr        emptyChar
  drawCell r c (Just p)
    | inEligibleGroup (r,c) = char (pieceToAttr p) (activeChar ctr)
    | otherwise             = char (pieceToAttr p) inactiveChar
  
  inEligibleGroup x = Set.size activeSet > 1 && Set.member x activeSet
    
boxImage :: Image -> Image
boxImage img = c '┌' <|> hbar <|> c '┐'
           <-> vbar  <|> img  <|> vbar
           <-> c '└' <|> hbar <|> c '┘'
  where
  w = image_width img
  h = image_height img
  c = char def_attr
  hbar = char_fill def_attr '─' w 1
  vbar = char_fill def_attr '│' 1 h
