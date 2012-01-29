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

main = bracket mkVty shutdown $ \vty -> do
  eventVar <- newMVar Refresh
  arr <- newBoard (10,20)
  gameVar  <- newIORef $ mkGameState (2,3) arr
  forkIO (clock eventVar)
  forkIO (drawingThread vty eventVar gameVar 0)
  
  let loop = do
        ev <- next_event vty
        case ev of
          EvKey KUp []    -> transform (moveCursor coordUp)
          EvKey KDown []  -> transform (moveCursor coordDown)
          EvKey KLeft []  -> transform (moveCursor coordLeft)
          EvKey KRight [] -> transform (moveCursor coordRight)
          EvKey KEsc []   -> return ()
          EvKey KEnter [] -> transform captureGroup
          _ -> loop

      transform f = do
        modifyIORef gameVar f
        putMVar eventVar Refresh
        loop
  loop


-- * Game board

type Coord = (Int,Int)

data GameState = GameState
  { cursorCoord :: Coord
  , boardArray :: Array Coord (Maybe Piece)
  , cachedConnectedSet :: Set Coord
  }

mkGameState c a = GameState
  { cursorCoord = c
  , boardArray  = a
  , cachedConnectedSet = connectedSet c a
  }

coordUp    (r,c) = (r-1,c)
coordDown  (r,c) = (r+1,c)
coordLeft  (r,c) = (r,c-1)
coordRight (r,c) = (r,c+1)

moveCursor direction st
  | rangeTest cursor' = mkGameState cursor' (boardArray st)
  | otherwise         = st
  where
  cursor' = direction (cursorCoord st)
  rangeTest = inRange (bounds (boardArray st))

moveDown st@GameState { cursorCoord = (r,c), boardArray = arr }
  | inRange (bounds arr) (r+1,c) = mkGameState (r+1,c) arr
  | otherwise                    = st

moveLeft st@GameState { cursorCoord = (r,c), boardArray = arr }
  | inRange (bounds arr) (r,c-1) = st { cursorCoord = (r,c-1) }
  | otherwise                    = st

moveRight st@GameState { cursorCoord = (r,c), boardArray = arr }
  | inRange (bounds arr) (r,c+1) = st { cursorCoord = (r,c+1) }
  | otherwise                    = st

captureGroup st
  | Set.size (cachedConnectedSet st) < 2 = st
  | otherwise      = mkGameState (cursorCoord st)
                        (collapseHoles $ boardArray st // [ (c, Nothing) | c <- Set.toList (cachedConnectedSet st) ])

arrayToList arr = filter (not . null) $ map (\c -> (mapMaybe (\r -> arr ! (r,c)) [rowHi,rowHi-1..rowLo])) [colLo..colHi]
  where
  ((rowLo, colLo),(rowHi,colHi)) = bounds arr

toCoords arr = concat . zipWith (\c -> zipWith (\r x -> ((r,c),x)) [rowHi,rowHi-1..rowLo]) [0..]
  where
  ((rowLo, colLo),(rowHi,colHi)) = bounds arr

collapseHoles arr = accumArray (\_ a -> Just a) Nothing (bounds arr) (toCoords arr (arrayToList arr))
  where
  ((rowLo, colLo),(rowHi,colHi)) = bounds arr


newBoard sz = liftM (listArray ((0,0),sz) . map Just) (replicateM n randomPiece)
  where
  n = (fst sz + 1) * (snd sz + 1)

connectedSet :: Coord -> Array Coord (Maybe Piece) -> Set Coord
connectedSet c arr = case arr ! c of
  Nothing -> Set.empty
  Just p -> aux (Set.singleton c) Set.empty
    where
    aux q res = case Set.minView q of
      Nothing -> res
      Just (x,q') | arr ! x /= Just p  -> aux q' res
                  | Set.member x res -> aux q' res
                  | otherwise        -> aux (foldr Set.insert q' (filter (inRange (bounds arr)) (neighbors x))) (Set.insert x res)

neighbors (x,y) = [(x+1,y),(x-1,y),(x,y-1),(x,y+1)]

-- * Drawing functions

{-
activeChar 0 = '▙'
activeChar 1 = '▛'
activeChar 2 = '▜'
activeChar 3 = '▟'

inactiveChar = '█'
-}

activeChar 0 = '⬖'
activeChar 1 = '⬘'
activeChar 2 = '⬗'
activeChar 3 = '⬙'
inactiveChar = '◆'

data RedrawEvent
  = TimerTick
  | Refresh

data Piece
  = Red
  | Purple
  | Green
  deriving (Eq, Show, Read)

randomPiece = do
  x <- randomRIO (0 :: Int, 2)
  return $! case x of
    0 -> Red
    1 -> Purple
    2 -> Green

colorToAttr Red    = with_fore_color def_attr red
colorToAttr Green  = with_fore_color def_attr green
colorToAttr Purple = with_fore_color def_attr magenta

colorToAttr' Red    = with_fore_color def_attr bright_red
colorToAttr' Green  = with_fore_color def_attr bright_green
colorToAttr' Purple = with_fore_color def_attr bright_magenta

clock eventVar = forever $ do
  threadDelay 250000
  putMVar eventVar TimerTick

drawingThread vty eventVar gameVar ctr = do
  ev <- takeMVar eventVar
  st <- readIORef gameVar
  let ctr' = (ctr + 1) `mod` 4
  case ev of
    TimerTick -> update vty (draw ctr' st) >> drawingThread vty eventVar gameVar ctr'
    Refresh   -> update vty (draw ctr st) >> drawingThread vty eventVar gameVar ctr

draw ctr st = Picture { pic_image = vert_cat [drawRow i | i <- [rowLo..rowHi]]
                      , pic_cursor = Cursor (fromIntegral (snd (cursorCoord st))) (fromIntegral (fst (cursorCoord st)))
                      , pic_background = Background ' ' def_attr }
  where
  activeSet = cachedConnectedSet st

  drawRow r = horiz_cat [drawCell r i (boardArray st ! (r,i)) | i <- [colLo..colHi]]

  drawCell _ _ Nothing = char def_attr ' '
  drawCell r c (Just p) | Set.size activeSet > 1 && Set.member (r,c) activeSet = char (colorToAttr' p) (activeChar ctr)
                        | otherwise              = char (colorToAttr p) inactiveChar

  ((rowLo, colLo),(rowHi,colHi)) = bounds (boardArray st)
