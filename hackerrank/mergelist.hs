{-# LANGUAGE AllowAmbiguousTypes #-}

import Data.List ( union, (\\), sortBy )
import GHC.OldList (sortOn)
import qualified GHC.Types
import Data.Function (on)
mergelists :: Ord a => [String] -> [String] -> [String] -> [String]
mergelists [] ys zs = reverse (sortNumeric ys \\ zs)
mergelists xs [] zs = reverse (sortNumeric xs \\ zs)
mergelists xs ys [] = reverse (sortNumeric xs `union` ys)
mergelists xs ys zs = reverse (sortNumeric ((xs `union` ys) \\ zs))

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

unionlists :: Eq a => [a] -> [a] -> [a]
unionlists [] [] = []
unionlists [] ys = ys
unionlists xs [] = xs
unionlists xs ys = xs `union` ys

lsort' :: [[a]] -> [[a]]
lsort' = sortOn length'

length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

sortNumeric :: [String] -> [String]
sortNumeric = sortBy (compare `on` length)
