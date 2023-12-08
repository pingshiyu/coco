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

  -- given a Store (s -> a) s, which accesses the `s` at a certain point,
  -- and so Store s a is isomorphic to the "index" of s. some kind of s[i]
  -- duplicate is such that for each "st", "extract (duplicate st) === st"
  -- duplicate (s[i]) ~= (s, s[i]), so it kind of breaks apart s[i] and its arguments
  duplicate :: Store s a -> Store s (Store s a)
  duplicate (Store f s) = Store (Store f) s

access :: [[a]] -> Int -> Int -> Store [[a]] a
access v i j = Store (\vv -> (vv !! i) !! j) v

data GameState = Empty | Alive | Dead
type Game = [[GameState]]

put :: GameState -> Int -> Int -> Game -> Game
put gs i j g =
  [ if i' /= i || j' /= j 
    then g !! i' !! j' 
    else gs 
    | j' <- [1 .. j], i' <- [1 .. i]]

initState :: Int -> Int -> Game
initState n m = replicate n (replicate m Empty)

move :: Game -> Game
move g = [access g i j | i <- [1 .. length g], j <- [1 .. length g]]

main :: IO ()
main = print "Life."