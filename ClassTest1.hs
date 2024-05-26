-- setting the "warn-incomplete-patterns" flag asks GHC to warn you
-- about possible missing cases in pattern-matching definitions
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- see https://wiki.haskell.org/Safe_Haskell
{-# LANGUAGE Safe #-}
module ClassTest1Retake (checkPeriodic, divisibleByIndex, findCubes, edit, edits,
solvable) where
import Data.List
import Data.Char
import Types
---------------------------------------------------------------------------------
---------------- DO **NOT** MAKE ANY CHANGES ABOVE THIS LINE --------------------
---------------------------------------------------------------------------------
{- Question 1 -}
checkPeriodic :: String -> Int -> Bool
checkPeriodic [] _ = False
checkPeriodic _ 0 = False
checkPeriodic s n = [s!!i| i<-[0..length s], i + n < length s] == [s!!(i+n)| i<-
[0..length s], i + n < length s]
{- Question 2 -}
divisibleByIndex :: [Int] -> [Bool]
divisibleByIndex [] = []
divisibleByIndex l = [ isDivisible (l!!y) (y+1) | y<-[0..(length l)-1]]
isDivisible :: Int -> Int -> Bool
isDivisible a b | a `mod` b == 0 = True
 |otherwise = False
{- Question 3 -}
findCubes :: Int -> [(Int,Int,Int)]
findCubes n = [(x,y,z) | x <-[1..n`div`3],
 y <-[x..n`div`3],
 z <-[y..n`div`3],
 x^3 + y^3 + z^3 == n]
{- Question 4 -}
edit :: EditCommand -> Text -> Text
edit c t = case c of
 Insert s -> (s:fst t, snd t)
 MoveLeft -> (drop 1 (fst t) , take 1 (fst t) ++ (snd t))
 MoveRight -> (take 1 (snd t) ++ (fst t), drop 1 (snd t))
 BackSpace -> (drop 1 (fst t), snd t)
edits :: [EditCommand] -> Text -> Text
edits [] t = t
edits (comm:comms) t = edits comms (edit comm t)

{- Question 5 -}
solvable :: ([Bool] -> Bool) -> Int -> Bool
solvable f n = any (==True) (map f (makeList n))
makeList :: Int -> [[Bool]]
makeList n = [replicate a True ++ replicate (n-a) False | a <- [0..n]]
--make lsit of booleans with length n
--check if there is a list of booleans length n that satisfy f