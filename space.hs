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
-- Towrite Coins replaces Coins, appends to Coins/Backs as needed
-- Towrite Subsets replaces Subsets, appends to Coins/Backs as needed
-- Toread Planes reads Planes
-- Toread Halfs reads Halfs
-- Toread Coins reads Coins
-- Toread Points calculates Points from Planes Coins
-- Toread Facets calculates Facets from Halfs Subsets Coins
-- Toread Subsets reads Subsets

type Planes = [Plane] -- per boundary
type Halfs = Space -- per boundary
type Subsets = [Region] -- in context of space
type Coins = [[Boundary]] -- per vertex
type Backs = Map [Boundary] Int

data State = State
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
 mainG (fromJust idx) (State [] [] [] [] empty)
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
mainH ifd (State a b c d e) (Change (ChangeA1 Type.Planes Type.Towrite idx siz) (ChangeA5B5 val)) = let
 numeric = mainHE idx a val
 symbolic = spaceFromPlanes 3 numeric
 in mainG ifd (mainHK symbolic numeric b c d e)
mainH ifd (State a b c d e) (Change (ChangeA1 Type.Halfs Type.Towrite idx siz) (ChangeA5B6 val)) = let
 symbolic = mainHF idx b val
 numeric = planesFromSpace 3 symbolic
 in mainG ifd (mainHK symbolic numeric b c d e)
mainH ifd (State a b c d e) (Change (ChangeA1 Type.Subsets Type.Towrite idx siz) (ChangeA5B10 val)) = let
 subsets = mainHG idx c val
 coins = mainHI b subsets d
 backs = mainHJ coins e
 in mainG ifd (mainHK b a b subsets coins backs)
mainH ifd (State a b c d e) (Change (ChangeA1 Type.Coins Type.Towrite idx siz) (ChangeA5B7 val)) = let
 coins = mainHH idx d val
 backs = mainHJ coins e
 in mainG ifd (mainHK b a b c coins backs)
mainH ifd state@(State a b c d e) (Change (ChangeA1 Type.Planes Type.Toread idx siz) (ChangeA5B5 val)) = do
 mainG ifd state
mainH ifd state@(State a b c d e) (Change (ChangeA1 Type.Halfs Type.Toread idx siz) (ChangeA5B6 val)) = do
 mainG ifd state
mainH ifd state@(State a b c d e) (Change (ChangeA1 Type.Subsets Type.Toread idx siz) (ChangeA5B10 val)) = do
 mainG ifd state
mainH ifd state@(State a b c d e) (Change (ChangeA1 Type.Coins Type.Toread idx siz) (ChangeA5B7 val)) = do
 mainG ifd state
mainH ifd state@(State a b c d e) (Change (ChangeA1 Type.Points Type.Toread idx siz) (ChangeA5B8 val)) = do
 mainG ifd state
mainH ifd state@(State a b c d e) (Change (ChangeA1 Type.Facets Type.Toread idx siz) (ChangeA5B9 val)) = do
 mainG ifd state
mainHE :: Int -> Planes -> [Scalar] -> Planes
mainHE = undefined -- replace indicated planes, filling in with default as needed
mainHF :: Int -> Halfs -> [Nested] -> Halfs
mainHF = undefined -- replace indicated boundaries, filling in with superspace as needed
mainHG :: Int -> Subsets -> [Int] -> Subsets
mainHG = undefined -- replace indicated regions, filling in with duplicates as needed
mainHH :: Int -> Coins -> [Listed] -> Coins
mainHH = undefined -- replace indicated boundary triplets, filling in with empties as needed
mainHI :: Halfs -> Subsets -> Coins -> Coins
mainHI = undefined -- append any boundary triplets of vertices on surface of subsets in halfs
mainHJ :: Coins -> Backs -> Backs
mainHJ = undefined -- insert any missing backreferences
mainHK :: Halfs -> Planes -> Halfs -> Subsets -> Coins -> Backs -> State
mainHK symbolic a b c d e = let
 place = spaceToPlace symbolic
 embed = embedSpace c (spaceToPlace b)
 regions = takeRegions embed place
 coins = mainHI symbolic regions d
 backs = mainHJ coins e
 in State a b regions coins backs

