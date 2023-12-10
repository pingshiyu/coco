{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ParallelListComp #-}

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE LambdaCase #-}

import Control.Comonad
import Data.Function
import Data.List

import Debug.Trace

data Store s a = Store (s -> a) s

instance Functor (Store s) where
    fmap f (Store g s) = Store (f . g) s

instance Comonad (Store s) where
  extract :: Store s a -> a
  extract (Store f s) = f s

  -- given a Store (s -> a) s, which accesses the `s` at a certain point,
  -- and so Store s a is isomorphic to the "index" of s. some kind of s[i]
  -- duplicate is such that for each "st", "extract (duplicate st) === st"
  -- duplicate (s[i]) ~= (s, s[i]), so it kind of breaks apart s[i] and its arguments
  duplicate :: Store s a -> Store s (Store s a)
  duplicate (Store f s) = Store (Store f) s

  -- extend :: (Store s a -> b) -> Store s a -> Store s b

data GameState = A | D deriving Eq
type Coord = (Int, Int)
type Game a = Store ([[GameState]], Coord) a
data Dir = Up | Down | Left | Right deriving (Eq, Show)

grid :: Game a -> [[GameState]]
grid (Store _ (gs, _)) = gs 

dirAsDelta :: Dir -> Coord
dirAsDelta = \case
    Up -> (-1, 0)
    Down -> (1, 0)
    Main.Left -> (0, -1)
    Main.Right -> (0, 1)

move :: Coord -> Dir -> Coord -> Coord
move (by, bx) d (iy, ix) =
    let (dy, dx) = dirAsDelta d
    in ((iy + dy) `mod` by, (ix + dx) `mod` bx)

go :: Dir -> Game a -> Game a
go d (Store f (gs, c)) = Store f (gs, move (length gs, length (head gs)) d c)

neighbours :: Game GameState -> [GameState]
neighbours g = [
    (extract . mf) g
    | mf <- [ go Main.Left . go Up
            , go Up
            , go Main.Right . go Up
            , go Main.Left
            , go Main.Right
            , go Main.Left . go Down
            , go Down
            , go Main.Right . go Down]]

countNeighbours :: GameState -> [GameState] -> Int
countNeighbours s = length . filter (==s)

life :: Game GameState -> GameState
life g
    | extract g == A && countNeighbours A (neighbours g) == 2 = A
    |                   countNeighbours A (neighbours g) == 3 = A -- new cell creation on dead or empty cell
    | otherwise                                               = D -- all other cases

dt :: Game GameState -> Game GameState
dt = extend life

instance Show GameState where
    show A = " @"
    show D = " ."

strBoard :: Game a -> String
strBoard g = (intercalate "\n" . map (intercalate "" . map show)) $ grid g

printBoard :: Game a -> IO ()
printBoard = putStrLn . strBoard

initGrid :: [[GameState]]
initGrid = [
    [D, D, D, D, D, D, D, D, D, D, D, D],
    [D, D, D, D, D, D, D, D, D, D, D, D],
    [D, D, A, A, D, D, D, D, D, D, D, D],
    [D, A, A, A, A, D, D, D, D, D, D, D],
    [D, D, A, A, D, D, D, D, D, D, D, D],
    [D, D, D, D, D, D, D, D, A, D, D, D],
    [D, D, D, D, D, D, D, D, D, D, D, D],
    [D, D, D, D, D, D, D, D, D, D, D, D],
    [D, D, D, D, D, D, D, D, D, D, D, D],
    [D, D, D, A, D, D, D, D, D, D, D, D],
    [D, D, D, A, A, D, D, D, D, D, D, D],
    [D, D, A, D, D, D, D, D, D, D, D, D],
    [D, D, D, D, D, D, D, D, D, A, D, D]
    ]

access :: ([[GameState]], (Int, Int)) -> GameState
access (gs, (iy, ix)) = gs !! iy !! ix

board :: Game GameState
board = Store access (initGrid, (0, 0))

main :: IO ()
main = let
    loop :: (Game GameState, Game GameState) -> IO ()
    loop (b0, b1) = if grid b0 == grid b1 then return () else do
        putStrLn ""
        printBoard b1
        loop (b1, dt b1)
    in do
        print "In the beginning there was..."
        printBoard board
        print ""
        printBoard (extend life board)
        loop (board, dt board)