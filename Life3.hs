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

  duplicate :: Store s a -> Store s (Store s a)
  duplicate (Store f s) = Store (Store f) s

  -- extend must work to turn _any_ Store s a into Store s b
  -- which means that `s` should not be dependent on any fixed `a`
  -- thus `s` should not store information (retrievable from the store interface) 
  -- based on specific `a`'s. So `s` should be purely for indexing purposes.

  -- extend :: (Store s a -> b) -> Store s a -> Store s b
  -- extend g st = fmap g (duplicate st)
  -- extend g (Store f (grid, (x, y))) = Store g 

data GameState = A | D deriving Eq
data Coord = Coord {y :: Int, x :: Int}
data Bounds = Bounds {h :: Int, w :: Int} -- (height, width) bounds
type Game a = Store (Bounds, Coord) a
data Dir = Up | Down | Left | Right deriving (Eq, Show)

dirAsDelta :: Dir -> Coord
dirAsDelta = \case
    Up -> Coord (-1) 0
    Down -> Coord 1 0
    Main.Left -> Coord 0 (-1)
    Main.Right -> Coord 0 1

move :: Bounds -> Dir -> Coord -> Coord
move b d c =
    let Coord dy dx = dirAsDelta d
    in Coord ((y c + dy) `mod` h b) ((x c + dx) `mod` w b)

go :: Dir -> Game a -> Game a
go d (Store f (b, c)) = Store f (b, move b d c)

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

grid :: Game a -> [[a]]
grid (Store f (b, _))= [
    [ f (b, Coord iy ix) 
    | ix <- [0..(w b - 1)]]
    | iy <- [0..(h b - 1)]]

strBoard :: Show a => Game a -> String
strBoard g = (intercalate "\n" . map (intercalate "" . map show)) $ grid g

printBoard :: Show a => Game a -> IO ()
printBoard = putStrLn . strBoard

initGrid :: [[GameState]]
initGrid = [
    [D, D, D, D, D],
    [D, A, D, D, D],
    [D, D, A, A, D],
    [D, A, A, A, A],
    [D, D, A, A, D]
    ]

access :: [[GameState]] -> (Bounds, Coord) -> GameState
access g (b, c) = g !! (y c `mod` h b) !! (x c `mod` w b)

board :: Game GameState
board = Store (access initGrid) (Bounds (length initGrid) (length (head initGrid)), Coord 0 0)

main :: IO ()
main = let
    loop :: (Game GameState, Game GameState) -> IO ()
    loop (b0, b1) = if grid b0 == grid b1 then return () else do
        putStrLn ""
        printBoard b1
        loop (b1, dt b1) 
        -- unfortunately runtime is now quadratic in the number of steps
        -- which blows up very quickly
    in do
        print "In the beginning there was..."
        printBoard board
        putStrLn ""
        printBoard (extend life board)
        loop (board, dt board)