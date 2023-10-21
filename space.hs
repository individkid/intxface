module Main where

import Naive
import Face
import Type
import System.Environment
import Data.IORef
import Data.Maybe
import Data.Map

-- "Planes", -- Scalar -- [Plane] -- per boundary
-- "Halfs", -- Nested -- Space -- per boundary
-- "Coins" -- Listed -- [[Boundary]] -- per vertex
-- "Points", -- Scalar -- [Point] -- per vertex
-- "Facets", -- Listed -- [[Incide]] -- per facet vertex triplets
-- "Subsets", -- Int -- [Region] -- in context of space

-- Towrite Planes replaces Planes, classifies Halfs, takes Subsets from old with new and old Halfs, appends to Coins/Backs as needed
-- Towrite Halfs replaces Halfs, samples Planes, takes Subsets from old with new and old Halfs, appends to Coins/Backs as needed
-- Towrite Coins replaces Coins filtered by Halfs, appends to Coins/Backs as needed
-- Towrite Subsets replaces Subsets filtered by Halfs, appends to Coins/Backs as needed
-- Toread Planes reads Planes
-- Toread Halfs reads Halfs
-- Toread Coins reads Coins
-- Toread Points calculates Points from Planes Coins
-- Toread Facets calculates Facets from Halfs Subsets Coins
-- Toread Subsets reads Subsets

newtype Incide = Incide Int deriving (Eq, Ord, Show) -- arbitrary identifier
newtype Facet = Facet Int deriving (Eq, Ord, Show) -- arbitrary identifier

type Planes = [Plane] -- per boundary
type Halfs = Space -- per boundary
type Subsets = [Region] -- in context of space
type Coins = [[Boundary]] -- per vertex
type Backs = Map [Boundary] Incide

data State = State
 Boundary
 Incide
 Facet
 Planes
 Halfs
 Subsets
 Coins
 Backs

main :: IO ()
main = getArgs >>= mainF

mainF :: [String] -> IO ()
mainF [a] = do
 idx <- wrapIdent Type.Spacez a
 mainG (fromJust idx) (State (Boundary 0) (Incide 0) (Facet 0) [] [] [] [] empty)
 return ()

mainG :: Int -> State -> IO ()
mainG ifd state = do
 change <- readChange ifd
 str <- newIORef ""
 showChange change str
 val <- readIORef str
 putStrLn val
 mainH ifd state change

mainH :: Int -> State -> Change -> IO ()
mainH _ _ (Change (ChangeA1 Type.Emergs _ _ _) _) = return ()
mainH ifd state (Change (ChangeA1 _ _ _ 0) _) = mainG ifd state
mainH ifd state (Change (ChangeA1 tag@Type.Planes act@Type.Towrite idx siz) (ChangeA5B5 (fst:lst))) = let
 nxt = Change (ChangeA1 tag act (idx + 1) (siz - 1)) (ChangeA5B5 lst)
 set = state -- Towrite Planes replaces Planes, classifies Halfs, takes Subsets from old with new and old Halfs, appends to Coins/Backs as needed
 in mainH ifd set nxt
mainH ifd state (Change (ChangeA1 tag@Type.Planes act@Type.Towrite idx siz) (ChangeA5B6 (fst:lst))) = let
 nxt = Change (ChangeA1 tag act (idx + 1) (siz - 1)) (ChangeA5B6 lst)
 set = state -- Towrite Halfs replaces Halfs, samples Planes, takes Subsets from old with new and old Halfs, appends to Coins/Backs as needed
 in mainH ifd set nxt
mainH ifd state (Change (ChangeA1 tag@Type.Planes act@Type.Towrite idx siz) (ChangeA5B7 (fst:lst))) = let
 nxt = Change (ChangeA1 tag act (idx + 1) (siz - 1)) (ChangeA5B7 lst)
 set = state -- Towrite Coins replaces Coins filtered by Halfs, appends to Coins/Backs as needed
 in mainH ifd set nxt
mainH ifd state (Change (ChangeA1 tag@Type.Planes act@Type.Towrite idx siz) (ChangeA5B10 (fst:lst))) = let
 nxt = Change (ChangeA1 tag act (idx + 1) (siz - 1)) (ChangeA5B10 lst)
 set = state -- Towrite Subsets replaces Subsets filtered by Halfs, appends to Coins/Backs as needed
 in mainH ifd set nxt
mainH ifd state (Change (ChangeA1 tag@Type.Planes act@Type.Toread idx siz) (ChangeA5B5 (fst:lst))) = let
 nxt = Change (ChangeA1 tag act (idx + 1) (siz - 1)) (ChangeA5B5 lst)
 set = state -- Toread Planes reads Planes
 in mainH ifd set nxt
mainH ifd state (Change (ChangeA1 tag@Type.Planes act@Type.Toread idx siz) (ChangeA5B6 (fst:lst))) = let
 nxt = Change (ChangeA1 tag act (idx + 1) (siz - 1)) (ChangeA5B6 lst)
 set = state -- Toread Halfs reads Halfs
 in mainH ifd set nxt
mainH ifd state (Change (ChangeA1 tag@Type.Planes act@Type.Toread idx siz) (ChangeA5B7 (fst:lst))) = let
 nxt = Change (ChangeA1 tag act (idx + 1) (siz - 1)) (ChangeA5B7 lst)
 set = state -- Toread Coins reads Coins
 in mainH ifd set nxt
mainH ifd state (Change (ChangeA1 tag@Type.Planes act@Type.Toread idx siz) (ChangeA5B8 (fst:lst))) = let
 nxt = Change (ChangeA1 tag act (idx + 1) (siz - 1)) (ChangeA5B8 lst)
 set = state -- Toread Points calculates Points from Planes Coins
 in mainH ifd set nxt
mainH ifd state (Change (ChangeA1 tag@Type.Planes act@Type.Toread idx siz) (ChangeA5B9 (fst:lst))) = let
 nxt = Change (ChangeA1 tag act (idx + 1) (siz - 1)) (ChangeA5B9 lst)
 set = state -- Toread Facets calculates Facets from Halfs Subsets Coins
 in mainH ifd set nxt
mainH ifd state (Change (ChangeA1 tag@Type.Planes act@Type.Toread idx siz) (ChangeA5B10 (fst:lst))) = let
 nxt = Change (ChangeA1 tag act (idx + 1) (siz - 1)) (ChangeA5B10 lst)
 set = state -- Toread Subsets reads Subsets
 in mainH ifd set nxt
