
module Trie
( Node (..)
, Trie (..)
, pretty
, empty
, nodeNum
, wordNum
, size
, ins
, bulkIns
, del
, bulkDel
, fromList
, toList
, find
) where

import Data.Semigroup
import Data.List (nub)
import Data.List.Split (splitOn)

type Node = Char

data Trie = Trie 
  { node  :: Node
  , child :: [Trie]
  , end   :: Bool
  } deriving (Show, Eq)

instance Semigroup Trie where
  (<>) t1 t2 = fromList $ (toList t1) ++ (toList t2)

instance Monoid Trie where
  mempty = empty
  mappend = (<>)

empty :: Trie
empty = Trie '.' [] False

nodeNum :: Trie -> Int
nodeNum (Trie '.' cs _) = 0 + sum (map nodeNum cs) -- Do not count root node.
nodeNum (Trie _ cs _) = 1 + sum (map nodeNum cs)

wordNum :: Trie -> Int
wordNum (Trie _ cs True) = 1 + sum (map wordNum cs)
wordNum (Trie _ cs False) = 0 + sum (map wordNum cs)

size :: Trie -> (Int, Int)
size trie = ((nodeNum trie), (wordNum trie))

ins :: String -> Trie -> Trie
ins [] (Trie n c e) = Trie n c True
ins (x:xs) t@(Trie n child e) = 
  if not $ x `elem` (map node child)
  then Trie n ((ins xs (Trie x [] ending)):child) e
  else Trie n [if node c /= x then c else (ins xs c) | c <- child] e
    where ending = if xs == [] then True else False

bulkIns :: [String] -> Trie -> Trie
bulkIns [] t = t
bulkIns (w:ws) t = bulkIns ws (ins w t)

del :: String -> Trie -> Trie
del x trie = fromList . filter (/= x) . toList $ trie

bulkDel :: [String] -> Trie -> Trie
bulkDel xs trie = fromList . filter (not . (`elem` xs)) . toList $ trie

fromList :: [String] -> Trie
fromList [] = empty
fromList xs = bulkIns (nub xs) empty

toList :: Trie -> [String]
toList trie = nub $ filter (not . null) $ splitOn "." $ parse "" trie

parse :: String -> Trie -> String
parse prefix trie@(Trie n cs end) = 
  let prefix' = prefix ++ [n]
  in 
    if null cs
    then prefix'
    else 
      let pfix = if end then prefix' ++ prefix' else prefix'
      in mconcat (map (parse pfix) cs)

postfixs :: String -> Trie -> [String]
postfixs [] _ = [] 
postfixs (x:xs) (Trie n cs _) = 
  let match = [c | c <- cs, x == node c]
  in 
    if null match
    then []
    else 
      if null xs
      then toList (Trie '.' (child (head match)) False)
      else postfixs xs (head match)

find :: String -> Trie -> [String]
find xs trie = map (xs ++) (postfixs xs trie)
