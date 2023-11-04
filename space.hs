module Main where

import Naive
import Face
import Type
import System.Environment
import Data.IORef
import Data.Maybe
import Data.Map
import Numeric.LinearAlgebra.Data

data State = State
 [Plane]
 Space
 [Region]
 [[Boundary]]
 (Map [Boundary] Int)

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
mainH fdx (State a b c d e) (Change (ChangeA1 Type.Subsets Type.Towrite idx siz) (ChangeA5B8 val)) = let
 subsets = intToSubset idx c val
 coins = mainI b subsets d
 backs = mainJ coins e
 in mainG fdx (State a b subsets coins backs)
mainH fdx (State a b c d e) (Change (ChangeA1 Type.Coins Type.Towrite idx siz) (ChangeA5B7 val)) = let
 coins = listedToCoin idx d val
 backs = mainJ coins e
 in mainG fdx (State a b c coins backs)
-- TODO Towrite Points changes Planes as needed
-- TODO Towrite Facets maintain facets in State
mainH fdx state@(State a b c d e) (Change (ChangeA1 Type.Planes Type.Toread idx siz) ChangeA5Bs) = do
 writeChange (Change (ChangeA1 Type.Planes Type.Toresp idx siz) (ChangeA5B5 val)) fdx
 mainG fdx state where
 val = planeToScalar idx siz a
mainH fdx state@(State a b c d e) (Change (ChangeA1 Type.Halfs Type.Toread idx siz) ChangeA5Bs) = do
 writeChange (Change (ChangeA1 Type.Halfs Type.Toresp idx siz) (ChangeA5B6 val)) fdx
 mainG fdx state where
 val = halfToNested idx siz b
mainH fdx state@(State a b c d e) (Change (ChangeA1 Type.Subsets Type.Toread idx siz) ChangeA5Bs) = do
 writeChange (Change (ChangeA1 Type.Subsets Type.Toresp idx siz) (ChangeA5B8 val)) fdx
 mainG fdx state where
 val = subsetToInt idx siz c
mainH fdx state@(State a b c d e) (Change (ChangeA1 Type.Coins Type.Toread idx siz) ChangeA5Bs) = do
 writeChange (Change (ChangeA1 Type.Coins Type.Toresp idx siz) (ChangeA5B7 val)) fdx
 mainG fdx state where
 val = coinToListed idx siz d
mainH fdx state@(State a b c d e) (Change (ChangeA1 Type.Points Type.Toread idx siz) ChangeA5Bs) = do
 writeChange (Change (ChangeA1 Type.Points Type.Toresp idx siz) (ChangeA5B9 val)) fdx
 mainG fdx state where
 val = mainM idx siz a d
mainH fdx state@(State a b c d e) (Change (ChangeA1 Type.Facets Type.Toread 0 0) ChangeA5Bs) = do
 writeChange (Change (ChangeA1 Type.Facets Type.Toresp 0 0) (ChangeA5B10 val)) fdx
 mainG fdx state where
 val = Prelude.map facetToListed (mainN b c e)
mainH fdx (State a b c d e) (Change (ChangeA1 Type.Emergs Type.Toadd idx 0) ChangeA5Bs) = let
 corners = mainN b c e
 (boundary,region) = mainL b c d (corners !! idx)
 regions = mainLG b boundary region c
 coins = mainI b regions d
 backs = mainJ coins e
 in mainG fdx (State a b regions coins backs)
mainH fdx (State a b c d e) (Change (ChangeA1 Type.Emergs Type.Tosub idx 0) ChangeA5Bs) = let
 corners = mainN b c e
 (boundary,region) = mainL b c d (corners !! idx)
 regions = mainLH region c
 coins = mainI b regions d
 backs = mainJ coins e
 in mainG fdx (State a b regions coins backs)
mainH _ _ (Change (ChangeA1 Type.Emergs _ _ _) _) = return ()

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

mainL :: Space -> [Region] -> [[Boundary]] -> [Int] -> (Boundary,Region)
mainL space regions coins corners = let
 triplets = Prelude.map (\x -> coins !! x) corners
 boundaries = Prelude.foldr (Naive.++) [] triplets
 shared = Prelude.foldr (Naive.+\) boundaries triplets
 attached = attachedRegions shared space
 in (head shared, head (Prelude.filter (mainLF (length boundaries) boundaries space) attached))
mainLF :: Int -> [Boundary] -> Space -> Region -> Bool
mainLF total boundaries space region = (length ((attachedBoundaries region space) Naive.+\ boundaries)) == total
mainLG :: Space -> Boundary -> Region -> [Region] -> [Region]
mainLG space boundary region regions = ((oppositeOfRegion [boundary] region space):regions)
mainLH :: Region -> [Region] -> [Region]
mainLH region regions = Prelude.filter (\x -> x /= region) regions

mainM :: Int -> Int -> [Plane] -> [[Boundary]] -> [Scalar]
-- find subcoins, find boundary triples, intersect to points, map to scalars
mainM idx siz planes coins = let
 nopoint = vectorToPoint (vector [0.0,0.0,0.0])
 subcoins = Prelude.take siz (Prelude.drop idx coins)
 triples = Prelude.map (\x -> Prelude.map (\(Boundary y) -> planes !! y) x) subcoins
 points = Prelude.map (\x -> fromMaybe nopoint (intersectPlanes 3 x)) triples
 in Prelude.map pointToScalar points

mainN :: Space -> [Region] -> (Map [Boundary] Int) -> [[Int]]
-- find triples of boundary triples, mapped to triples of coin indices
mainN space regions backs = let
 boundaries = boundariesOfSpace space
 pairs = concat (Prelude.map (\x -> Prelude.map (\y -> (x,y)) (attachedBoundaries x space)) regions)
 polygons = Prelude.filter (\(x,y) -> not (Prelude.elem (oppositeOfRegion [y] x space) regions)) pairs
 triangles = concat (Prelude.map (mainNG space) (Prelude.map (mainNF space regions) polygons))
 in map2 (\x -> fromMaybe 0 (Data.Map.lookup x backs)) triangles
mainNF :: Space -> [Region] -> (Region,Boundary) -> [[Boundary]]
-- find corners of polygon
mainNF space regions (region,boundary) = let
 attached = attachedBoundaries region space
 triples = Prelude.filter (\x -> Prelude.elem boundary x) (subsets 2 attached)
 corners = Prelude.filter (\x -> oppositeOfRegionExists x region space) triples
 in Prelude.map (Prelude.filter (\x -> x /= boundary)) corners
mainNG :: Space -> [[Boundary]] -> [[[Boundary]]]
-- find triangles of corners
mainNG _ [first,second,third] = [[first,second,third]]
mainNG space corners = let
 (first:second:rest) = corners
 ordered = Prelude.zip [0..] (mainNH corners)
 one = Prelude.foldr (\(x,y) z -> if (y == first) then x else z) 0 ordered
 other = Prelude.foldr (\(x,y) z -> if (y == second) then x else z) 0 ordered
 before = Prelude.map snd (Prelude.filter (\(x,_) -> x == other || x <= one) ordered)
 after = Prelude.map snd (Prelude.filter (\(x,_) -> x == other || x >= one) ordered)
 in (mainNG space before) Prelude.++ (mainNG space after)
mainNH :: [[Boundary]] -> [[Boundary]]
-- successor shares boundaries with predecessor
mainNH vertices = let
 boundaries = Prelude.foldr (Naive.++) [] vertices
 (boundary:_) = Prelude.foldr (Naive.+\) boundaries vertices
 ((one:others):rest) = Prelude.map (\x -> x Naive.\\ [boundary]) vertices
 corners = mainNK (mainNI ((one:others):rest)) (one,(one:others)) (one,(one:others)) []
 in Prelude.map (\x -> boundary:x) corners
mainNI :: [[Boundary]] -> (Map Boundary [[Boundary]])
-- map boundaries of corners to corner pairs
mainNI corners = Prelude.foldr (\x a -> Prelude.foldr (\y b -> mainNJ y x (Data.Map.lookup y b) b) a x) empty corners
mainNJ :: Boundary -> [Boundary] -> (Maybe [[Boundary]]) -> (Map Boundary [[Boundary]]) -> (Map Boundary [[Boundary]])
mainNJ boundary corner (Just sofar) backref = Data.Map.insert boundary (corner:sofar) backref
mainNJ boundary corner Nothing backref = Data.Map.insert boundary [corner] backref
mainNK :: (Map Boundary [[Boundary]]) -> (Boundary,[Boundary]) -> (Boundary,[Boundary]) -> [[Boundary]] -> [[Boundary]]
-- recurse to find next tuple to append to list
mainNK backref first next@(boundary,corner) done
 | next == first = done
 | otherwise = mainNK backref first (other,found) (found:done) where
 found = Prelude.head (Prelude.filter (\x -> x /= corner) (fromMaybe [] (Data.Map.lookup boundary backref)))
 other = Prelude.head (Prelude.filter (\x -> x /= boundary) found)

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
facetToListed x = Listed (ListedA1 (length x) x)
listedToFacet :: Listed -> [Int]
listedToFacet = undefined

