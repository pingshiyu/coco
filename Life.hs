{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BlockArguments #-}

{-# OPTIONS_GHC -Wall #-}

import Control.Comonad

data Store s a = Store (s -> a) s

instance Functor (Store s) where
    fmap f (Store g s) = Store (f . g) s

instance Comonad (Store s) where
  extract :: Store s a -> a
  extract (Store f s) = f s
  duplicate :: Store s a -> Store s (Store s a)
  duplicate (Store f s) = Store (Store f) s

access :: [[a]] -> Int -> Int -> Store [[a]] a
access v i j = Store (\vv -> (vv !! i) !! j) v

data GameState = Empty | Alive | Dead
type Game = [[GameState]]

put :: GameState -> Int -> Int -> Game -> Game
put gs i j g = [
    [ if i' /= i || j' /= j 
        then g !! i' !! j' 
        else gs 
        | j' <- [1 .. j]
    ]   | i' <- [1 .. i]]

initState :: Int -> Int -> Game
initState n m = replicate n (replicate m Empty)

move :: Game -> Game
-- move = access i j 

main :: IO ()
main = print "Life."