{-# LANGUAGE BangPatterns #-}
module Main (
    main
) where

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Set as S
import Data.Functor
import Data.List

type Node = S.Set Int
type Tree = V.Vector Node

getNodes :: String -> [Int]
getNodes edgeStr = map convertToNode $ words edgeStr
  where convertToNode = ((-1) +) . read

fill tree [] = return ()
fill tree (edgeStr:edgeStrs) = do
    [set1, set2] <- mapM (VM.unsafeRead tree) nodes
    let !newSet1 = node2 `S.insert` set1
        !newSet2 = node1 `S.insert` set2
    VM.unsafeWrite tree node1 newSet1
    VM.unsafeWrite tree node2 newSet2
    fill tree edgeStrs
  where [node1, node2] = nodes
        nodes = getNodes edgeStr

parseTree :: [String] -> Int -> IO Tree
parseTree edgeStrs n = do
    tree <- VM.replicate n S.empty
    fill tree edgeStrs
    V.unsafeFreeze tree

main :: IO ()
main = do
  header:edges <- lines <$> readFile "tree.txt"
  let [n] = map read $ words header
  tree <- parseTree edges n
  print $ tree V.! 0
