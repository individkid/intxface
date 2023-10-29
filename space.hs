module Main where

import Naive
import Face
import Type
import System.Environment
import Data.IORef
import Data.Maybe
import Data.Map
import Numeric.LinearAlgebra.Data

-- "Planes", -- Scalar -- [Plane] -- per boundary
-- "Halfs", -- Nested -- Space -- per boundary
-- "Coins" -- Listed -- [[Boundary]] -- per vertex
-- "Points", -- Scalar -- [Point] -- per vertex
-- "Facets", -- Listed -- [[Int]] -- per facet vertex triplets
-- "Subsets", -- Int -- [Region] -- in context of space

-- Towrite Planes replaces Planes, classifies Halfs, takes Subsets from old with new and old Halfs, appends to Coins/Backs as needed
-- Towrite Halfs replaces Halfs, samples Planes, takes Subsets from old with new and old Halfs, appends to Coins/Backs as needed
-- Towrite Coins replaces Coins, appends to Coins/Backs as needed
-- TODO Towrite Points replaces Points, reconstructs Planes, classifies Halfs, takes Subsets from old with new and old Halfs, appends to Coins/Backs as needed
-- TODO Toadd Facets additives Subsets, appends to Coins/Backs as needed
-- TODO Tosub Facets subtractives Subsets, appends to Coins/Backs as needed
-- Towrite Subsets replaces Subsets, appends to Coins/Backs as needed
-- Toread Planes reads Planes
-- Toread Halfs reads Halfs
-- Toread Coins reads Coins
-- Toread Points calculates Points from Planes Coins
-- Toread Facets calculates Facets from Halfs Subsets Coins
-- Toread Subsets reads Subsets

data State = State
 [Plane] -- scalarToPlane planeToScalar
 Space -- nestedToHalf halfToNested
 [Region] -- intToSubset subsetToInt
 [[Boundary]] -- listedToCoin coinToListed
 (Map [Boundary] Int)
 -- [Point] -- pointToScalar
 -- [[Int]] -- facetToListed

main :: IO ()
main = getArgs >>= mainF

mainF :: [String] -> IO ()
mainF [a] = do
 idx <- wrapIdent Type.Spacez a
 mainG (fromJust idx) (State [] [] [] [] empty)
 return ()

mainG :: Int -> State -> IO ()
mainG fdx state = do
 change <- readChange fdx
 str <- newIORef ""
 showChange change str
 val <- readIORef str
 putStrLn val
 mainH fdx state change

mainH :: Int -> State -> Change -> IO ()
mainH _ _ (Change (ChangeA1 Type.Emergs _ _ _) _) = return ()
mainH fdx (State a b c d e) (Change (ChangeA1 Type.Planes Type.Towrite idx siz) (ChangeA5B5 val)) = let
 numeric = scalarToPlane idx a val
 symbolic = spaceFromPlanes 3 numeric
 subsets = mainK symbolic b c
 coins = mainI symbolic subsets d
 backs = mainJ coins e
 in mainG fdx (State numeric symbolic subsets coins backs)
mainH fdx (State a b c d e) (Change (ChangeA1 Type.Halfs Type.Towrite idx siz) (ChangeA5B6 val)) = let
 symbolic = nestedToHalf idx b val
 numeric = planesFromSpace 3 symbolic
 subsets = mainK symbolic b c
 coins = mainI symbolic subsets d
 backs = mainJ coins e
 in mainG fdx (State numeric symbolic subsets coins backs)
mainH fdx (State a b c d e) (Change (ChangeA1 Type.Subsets Type.Towrite idx siz) (ChangeA5B10 val)) = let
 subsets = intToSubset idx c val
 coins = mainI b subsets d
 backs = mainJ coins e
 in mainG fdx (State a b subsets coins backs)
mainH fdx (State a b c d e) (Change (ChangeA1 Type.Coins Type.Towrite idx siz) (ChangeA5B7 val)) = let
 coins = listedToCoin idx d val
 backs = mainJ coins e
 in mainG fdx (State a b c coins backs)
mainH fdx state@(State a b c d e) (Change (ChangeA1 Type.Planes Type.Toread idx siz) ChangeA5Bs) = do
 writeChange (Change (ChangeA1 Type.Planes Type.Toresp idx siz) (ChangeA5B5 val)) fdx
 mainG fdx state where
 val = planeToScalar idx siz a
mainH fdx state@(State a b c d e) (Change (ChangeA1 Type.Halfs Type.Toread idx siz) ChangeA5Bs) = do
 writeChange (Change (ChangeA1 Type.Halfs Type.Toresp idx siz) (ChangeA5B6 val)) fdx
 mainG fdx state where
 val = halfToNested idx siz b
mainH fdx state@(State a b c d e) (Change (ChangeA1 Type.Subsets Type.Toread idx siz) ChangeA5Bs) = do
 writeChange (Change (ChangeA1 Type.Subsets Type.Toresp idx siz) (ChangeA5B10 val)) fdx
 mainG fdx state where
 val = subsetToInt idx siz c
mainH fdx state@(State a b c d e) (Change (ChangeA1 Type.Coins Type.Toread idx siz) ChangeA5Bs) = do
 writeChange (Change (ChangeA1 Type.Coins Type.Toresp idx siz) (ChangeA5B7 val)) fdx
 mainG fdx state where
 val = coinToListed idx siz d
mainH fdx state@(State a b c d e) (Change (ChangeA1 Type.Points Type.Toread idx siz) ChangeA5Bs) = do
 writeChange (Change (ChangeA1 Type.Points Type.Toresp idx siz) (ChangeA5B8 val)) fdx
 mainG fdx state where
 val = mainM idx siz a d
mainH fdx state@(State a b c d e) (Change (ChangeA1 Type.Facets Type.Toread 0 0) ChangeA5Bs) = do
 writeChange (Change (ChangeA1 Type.Facets Type.Toresp 0 0) (ChangeA5B9 val)) fdx
 mainG fdx state where
 val = mainN b c e

mainI :: Space -> [Region] -> [[Boundary]] -> [[Boundary]]
-- append any boundary triplets of vertices on surface of subsets in halfs
mainI space regions coins = let
 tofold = Prelude.map (mainIF space regions) regions
 in Prelude.foldr (Prelude.++) coins tofold
mainIF :: Space -> [Region] -> Region -> [[Boundary]]
mainIF space regions region = let
 attached = attachedBoundaries region space
 triplets = subsets 3 attached
 in Prelude.filter (mainIG space regions) triplets
mainIG :: Space -> [Region] -> [Boundary] -> Bool
mainIG space regions boundaries = let
 attached = attachedRegions boundaries space
 shared = Prelude.filter (\x -> Prelude.elem x regions) attached
 count = Prelude.length shared
 in (count /= 0) && (count /= (Prelude.length regions))

mainJ :: [[Boundary]] -> (Map [Boundary] Int) -> (Map [Boundary] Int)
-- insert any missing backreferences
mainJ coins backs = Prelude.foldr (\(x,y) z -> insert y x z) backs (Prelude.zip [0..] coins)

mainK :: Space -> Space -> [Region] -> [Region]
mainK symbolic b c = let
 place = spaceToPlace symbolic
 embed = embedSpace c (spaceToPlace b)
 in takeRegions embed place

mainM :: Int -> Int -> [Plane] -> [[Boundary]] -> [Scalar]
-- find subcoins, find boundary triples, intersect to points, map to scalars
mainM idx siz planes coins = let
 nopoint = vectorToPoint (vector [0.0,0.0,0.0])
 subcoins = Prelude.take siz (Prelude.drop idx coins)
 triples = Prelude.map (\x -> Prelude.map (\(Boundary y) -> planes !! y) x) subcoins
 points = Prelude.map (\x -> fromMaybe nopoint (intersectPlanes 3 x)) triples
 in Prelude.map pointToScalar points

mainN :: Space -> [Region] -> (Map [Boundary] Int) -> [Listed]
-- find triples of boundary triples, mapped to triples of coin indices
mainN space regions backs = let
 boundaries = boundariesOfSpace space
 pairs = concat (Prelude.map (\x -> Prelude.map (\y -> (x,y)) (attachedBoundaries x space)) regions)
 polygons = Prelude.filter (\(x,y) -> not (Prelude.elem (oppositeOfRegion [y] x space) regions)) pairs
 triangles = concat (Prelude.map (mainNG space) (Prelude.map (mainNF space regions) polygons))
 in Prelude.map (\x -> Listed (ListedA1 (length x) x)) (map2 (\x -> fromMaybe 0 (Data.Map.lookup x backs)) triangles)
mainNF :: Space -> [Region] -> (Region,Boundary) -> [[Boundary]]
-- find corners of polygon
mainNF space regions (region,boundary) = let
 attached = attachedBoundaries region space
 triples = Prelude.filter (\x -> Prelude.elem boundary x) (subsets 2 attached)
 in Prelude.filter (\x -> oppositeOfRegionExists x region space) triples
mainNG :: Space -> [[Boundary]] -> [[[Boundary]]]
-- find triangles of corners
mainNG space corners = let
 (one:other:rest) = corners
 in Prelude.map (\x -> [one,other,x]) rest -- note how easy this is

scalarToPlane :: Int -> [Plane] -> [Scalar] -> [Plane]
scalarToPlane = undefined -- replace indicated planes, filling in with default as needed
nestedToHalf :: Int -> Space -> [Nested] -> Space
nestedToHalf = undefined -- replace indicated boundaries, filling in with superspace as needed
intToSubset :: Int -> [Region] -> [Int] -> [Region]
intToSubset = undefined -- replace indicated regions, filling in with duplicates as needed
listedToCoin :: Int -> [[Boundary]] -> [Listed] -> [[Boundary]]
listedToCoin = undefined -- replace indicated boundary triplets, filling in with empties as needed
planeToScalar :: Int -> Int -> [Plane] -> [Scalar]
planeToScalar = undefined -- take and drop
halfToNested :: Int -> Int -> Space -> [Nested]
halfToNested = undefined
subsetToInt :: Int -> Int -> [Region] -> [Int]
subsetToInt = undefined
coinToListed :: Int -> Int -> [[Boundary]] -> [Listed]
coinToListed = undefined
pointToScalar :: Point -> Scalar
pointToScalar = undefined
facetToListed :: [Int] -> Listed
facetToListed = undefined

