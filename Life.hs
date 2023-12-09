{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ParallelListComp #-}

{-# OPTIONS_GHC -Wall #-}

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
type Game = [[GameState]]

-- neighbours with wraparound on a nxm grid
neighbours ::  Int -> Int -> Game -> [[GameState]]
neighbours i j grid = let
    n = length grid
    m = length (head grid)
    out = [[
        grid !! ((i + dy) `mod` n) !! ((j + dx) `mod` m)
        | dx <- [-1 .. 1]]
        | dy <- [-1 .. 1]]
    in out
        -- trace ("i=" ++ show i ++ " j=" ++ show j ++ " mid=" ++ show (out !! 1 !! 1) ++ " neis=" ++ show (countNeighbours A out) ++ "\n" ++ strBoard out ++ "\n") out

access :: Int -> Int -> Game -> Store Game [[GameState]]
access i j = Store (neighbours i j)

countNeighbours :: GameState -> [[GameState]] -> Int
countNeighbours s v = sum $ map (length . filter (==s)) n
    where n = [[v !! i !! j
            | j <- [0 .. 2], not (i == 1 && j == 1)]
            | i <- [0 .. 2]]

is :: [[GameState]] -> GameState -> Bool
is n = (==) (n !! 1 !! 1)

life :: Store Game [[GameState]] -> GameState
life s
    | extract s `is` A && countNeighbours A (extract s) <  2 = D
    | extract s `is` A && countNeighbours A (extract s) == 2 = A
    |                     countNeighbours A (extract s) == 3 = A -- new cell creation on dead or empty cell
    | extract s `is` A && countNeighbours A (extract s) >  3 = D
    | otherwise                                              = D

move :: Game -> Game
move g = [[(life `extend` access i j g) & extract
        | j <- [0 .. length (head g) - 1]]
        | i <- [0 .. length g - 1]]

instance Show GameState where
    show A = " @"
    show D = " ."

strBoard :: Game -> String
strBoard = intercalate "\n" . map (intercalate "" . map show)

printBoard :: Game -> IO ()
printBoard = putStrLn . strBoard

board :: Game
board = [
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

main :: IO ()
main = let
    loop :: (Game, Game) -> IO ()
    loop (b0, b1) = if b0 == b1 then return () else do
        putStrLn ""
        printBoard b1
        loop (b1, move b1)
    in do
        print "In the beginning there was..."
        printBoard board
        loop (board, move board)