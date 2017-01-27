{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiWayIf #-}
module Main where

import UI.NCurses
import Control.Monad.IO.Class(liftIO)
import Control.Monad(unless, liftM2, ap)
import System.Environment(getArgs)
import Data.List(sort)
import Prelude hiding (Either(..))

type Coord = (Int, Int)

data Move = Up | Left | Right | Down

data Level = Level { walls :: [Coord]
                   , storage :: [Coord]
                   , crates :: [Coord]
                   , player :: Coord
                   , width :: Int
                   , height :: Int
                   }

instance Show Level where
    show = unlines . levelToStrings

levelToStrings :: Level -> [String]
levelToStrings level@Level{width,height} =
    [[symbolAt level (x, y) | x <- [0..width]] | y <- [0..height]]

symbolAt :: Level -> Coord -> Char
symbolAt level coord
    | isPlayerOnStorage level coord = '+'
    | isPlayer level coord = '@'
    | isWall level coord = '#'
    | isFilledStorage level coord = '*'
    | isCrate level coord = 'o'
    | isStorage level coord = '.'
    | otherwise = ' '

isPlayerOnStorage :: Level -> Coord -> Bool
isPlayerOnStorage = ap (liftM2 (&&) . isStorage) isPlayer

isPlayer :: Level -> Coord -> Bool
isPlayer = (==) . player

isWall :: Level -> Coord -> Bool
isWall = flip elem . walls

isStorage :: Level -> Coord -> Bool
isStorage = flip elem . storage

isCrate :: Level -> Coord -> Bool
isCrate = flip elem . crates

isFilledStorage :: Level -> Coord -> Bool
isFilledStorage = ap (liftM2 (&&) . isStorage) isCrate

loadLevel :: String -> IO Level
loadLevel = (levelFromString <$>) . readFile

levelFromString :: String -> Level
levelFromString str =
    Level { walls
          , storage
          , crates
          , player
          , width
          , height
          }
  where
    levelDataWithCoords = zipWithCoords str
    walls = coordsOf '#' levelDataWithCoords
    storage = sort $ coordsOf '.' levelDataWithCoords
    crates = sort $ coordsOf 'o' levelDataWithCoords
    player = head $ coordsOf '@' levelDataWithCoords
    width = maxLengthOf str
    height = length . lines $ str

maxLengthOf :: String -> Int
maxLengthOf = maximum . map length . lines

zipWithCoords :: String -> [(Char, Coord)]
zipWithCoords str = concat $ zipWith zip (lines str) [[(x, y) | x <- [0..]] | y <- [0..]]

coordsOf :: Char -> [(Char, Coord)] -> [Coord]
coordsOf c = map snd . filter ((== c) . fst)

runGameLoop :: Window -> Level -> Curses ()
runGameLoop window level = do
    updateWindow window $ renderLevel level
    render
    move <- liftIO getMove
    let level' = performMove level move in
        if hasWon level'
           then notifyPlayer window level'
           else runGameLoop window level'

hasWon :: Level -> Bool
hasWon = liftM2 (==) storage crates

notifyPlayer :: Window -> Level -> Curses ()
notifyPlayer window level = do
    updateWindow window $ do
      renderLevel level
      drawString "YOU WON (q to exit)"
    render
    waitFor window (== EventCharacter 'q')

renderLevel :: Level -> Update ()
renderLevel = (clear >>) . (moveCursor 0 0 >>) . drawString . show

waitFor :: Window -> (Event -> Bool) -> Curses ()
waitFor window predicate = loop
  where
    loop = do
      event <- getEvent window Nothing
      case event of
           Nothing -> loop
           Just event' -> unless (predicate event') loop

charToMove :: Char -> Maybe Move
charToMove 'k' = Just Up
charToMove 'h' = Just Left
charToMove 'j' = Just Down
charToMove 'l' = Just Right
charToMove _ = Nothing

getMove :: IO Move
getMove = do
    char <- getChar
    case charToMove char of
         Just x -> return x
         Nothing -> getMove

performMove :: Level -> Move -> Level
performMove level@Level{player} move =
    if | isWall level newPos -> level
       | isCrate level newPos ->
         if isWall level newPos' || isCrate level newPos'
            then level
            else moveCrate level' newPos newPos'
       | otherwise -> level'
  where
    newPos = moveCoord player move
    newPos' = moveCoord newPos move
    level' = level { player = newPos }

moveCrate :: Level -> Coord -> Coord -> Level
moveCrate level@Level{crates} oldPos newPos =
    level { crates = crates' }
  where
    crates' = sort $ newPos : filter (/= oldPos) crates

moveCoord :: Coord -> Move -> Coord
moveCoord (x, y) Up = (x, y - 1)
moveCoord (x, y) Down = (x, y + 1)
moveCoord (x, y) Left = (x - 1, y)
moveCoord (x, y) Right = (x + 1, y)

main :: IO ()
main = runCurses $ do
    setEcho False
    setCursorMode CursorInvisible
    w <- defaultWindow
    args <- liftIO getArgs
    case args of
         [filename] -> do
             level <- liftIO $ loadLevel filename
             runGameLoop w level
         _ -> error "USAGE: sokoban FILENAME"
