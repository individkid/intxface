module Main where

import Naive
import Face
import Type
import System.Environment
import Data.IORef
import Data.Maybe
import qualified Data.Map as Map
import qualified Numeric.LinearAlgebra.Data as Matrix
import qualified GHC.Float as GHC

data State = State
 [Plane]
 Space
 [Region]
 [[Boundary]]
 (Map.Map [Boundary] Int)

main :: IO ()
main = getArgs >>= mainF

mainF :: [String] -> IO ()
mainF [a] = do
 idx <- wrapIdent Type.Spacez a
 mainG (fromJust idx) (State [] [] [] [] Map.empty)
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
 val = map facetToListed (mainN b c e)
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
 tofold = map (mainIF space regions) regions
 in foldr (Prelude.++) coins tofold
mainIF :: Space -> [Region] -> Region -> [[Boundary]]
mainIF space regions region = let
 attached = attachedBoundaries region space
 triplets = subsets 3 attached
 in filter (mainIG space regions) triplets
mainIG :: Space -> [Region] -> [Boundary] -> Bool
mainIG space regions boundaries = let
 attached = attachedRegions boundaries space
 shared = filter (\x -> elem x regions) attached
 count = length shared
 in (count /= 0) && (count /= (length regions))

mainJ :: [[Boundary]] -> (Map.Map [Boundary] Int) -> (Map.Map [Boundary] Int)
-- insert any missing backreferences
mainJ coins backs = foldr (\(x,y) z -> Map.insert y x z) backs (zip [0..] coins)

mainK :: Space -> Space -> [Region] -> [Region]
mainK symbolic b c = let
 place = spaceToPlace symbolic
 embed = embedSpace c (spaceToPlace b)
 in takeRegions embed place

mainL :: Space -> [Region] -> [[Boundary]] -> [Int] -> (Boundary,Region)
mainL space regions coins corners = let
 triplets = map (\x -> coins !! x) corners
 boundaries = foldr (Naive.++) [] triplets
 shared = foldr (Naive.+\) boundaries triplets
 attached = attachedRegions shared space
 in (head shared, head (filter (mainLF (length boundaries) boundaries space) attached))
mainLF :: Int -> [Boundary] -> Space -> Region -> Bool
mainLF total boundaries space region = (length ((attachedBoundaries region space) Naive.+\ boundaries)) == total
mainLG :: Space -> Boundary -> Region -> [Region] -> [Region]
mainLG space boundary region regions = ((oppositeOfRegion [boundary] region space):regions)
mainLH :: Region -> [Region] -> [Region]
mainLH region regions = filter (\x -> x /= region) regions

mainM :: Int -> Int -> [Plane] -> [[Boundary]] -> [Scalar]
-- find subcoins, find boundary triples, intersect to points, map to scalars
mainM idx siz planes coins = let
 nopoint = vectorToPoint (Matrix.vector [0.0,0.0,0.0])
 subcoins = take siz (drop idx coins)
 triples = map (\x -> map (\(Boundary y) -> planes !! y) x) subcoins
 points = map (\x -> fromMaybe nopoint (intersectPlanes 3 x)) triples
 in map pointToScalar points

mainN :: Space -> [Region] -> (Map.Map [Boundary] Int) -> [[Int]]
-- find triples of boundary triples, mapped to triples of coin indices
mainN space regions backs = let
 boundaries = boundariesOfSpace space
 pairs = concat (map (\x -> map (\y -> (x,y)) (attachedBoundaries x space)) regions)
 polygons = filter (\(x,y) -> not (elem (oppositeOfRegion [y] x space) regions)) pairs
 triangles = concat (map (mainNG space) (map (mainNF space regions) polygons))
 in map2 (\x -> fromMaybe 0 (Map.lookup x backs)) triangles
mainNF :: Space -> [Region] -> (Region,Boundary) -> [[Boundary]]
-- find corners of polygon
mainNF space regions (region,boundary) = let
 attached = attachedBoundaries region space
 triples = filter (\x -> elem boundary x) (subsets 2 attached)
 corners = filter (\x -> oppositeOfRegionExists x region space) triples
 in map (filter (\x -> x /= boundary)) corners
mainNG :: Space -> [[Boundary]] -> [[[Boundary]]]
-- find triangles of corners
mainNG _ [first,second,third] = [[first,second,third]]
mainNG space corners = let
 (first:second:rest) = corners
 ordered = zip [0..] (mainNH corners)
 one = foldr (\(x,y) z -> if (y == first) then x else z) 0 ordered
 other = foldr (\(x,y) z -> if (y == second) then x else z) 0 ordered
 before = map snd (filter (\(x,_) -> x == other || x <= one) ordered)
 after = map snd (filter (\(x,_) -> x == other || x >= one) ordered)
 in (mainNG space before) Prelude.++ (mainNG space after)
mainNH :: [[Boundary]] -> [[Boundary]]
-- successor shares boundaries with predecessor
mainNH vertices = let
 boundaries = foldr (Naive.++) [] vertices
 (boundary:_) = foldr (Naive.+\) boundaries vertices
 ((one:others):rest) = map (\x -> x Naive.\\ [boundary]) vertices
 corners = mainNK (mainNI ((one:others):rest)) (one,(one:others)) (one,(one:others)) []
 in map (\x -> boundary:x) corners
mainNI :: [[Boundary]] -> (Map.Map Boundary [[Boundary]])
-- map boundaries of corners to corner pairs
mainNI corners = foldr (\x a -> foldr (\y b -> mainNJ y x (Map.lookup y b) b) a x) Map.empty corners
mainNJ :: Boundary -> [Boundary] -> (Maybe [[Boundary]]) -> (Map.Map Boundary [[Boundary]]) -> (Map.Map Boundary [[Boundary]])
mainNJ boundary corner (Just sofar) backref = Map.insert boundary (corner:sofar) backref
mainNJ boundary corner Nothing backref = Map.insert boundary [corner] backref
mainNK :: (Map.Map Boundary [[Boundary]]) -> (Boundary,[Boundary]) -> (Boundary,[Boundary]) -> [[Boundary]] -> [[Boundary]]
-- recurse to find next tuple to append to list
mainNK backref first next@(boundary,corner) done
 | next == first = done
 | otherwise = mainNK backref first (other,found) (found:done) where
 found = head (filter (\x -> x /= corner) (fromMaybe [] (Map.lookup boundary backref)))
 other = head (filter (\x -> x /= boundary) found)

scalarToPlane :: Int -> [Plane] -> [Scalar] -> [Plane]
scalarToPlane idx planes scalars
 | lim <= num = take idx planes Prelude.++ rep Prelude.++ drop lim planes
 | idx <= num = take idx planes Prelude.++ rep
 | otherwise = undefined where
 num = length planes; lim = idx + (length scalars)
 rep = map (\(Scalar (ScalarA1 floats)) -> Matrix.fromList (map GHC.float2Double floats)) scalars
nestedToHalf :: Int -> Space -> [Nested] -> Space
nestedToHalf idx space nesteds
 | lim <= num = take idx space Prelude.++ rep Prelude.++ drop lim space
 | idx <= num = take idx space Prelude.++ rep
 | otherwise = undefined where
 num = length space; lim = idx + (length nesteds)
 rep = map (\(Nested (NestedA1 _ listeds)) -> map (\(Listed (ListedA1 _ vals)) -> map Region vals) listeds) nesteds
intToSubset :: Int -> [Region] -> [Int] -> [Region]
intToSubset idx subset vals
 | lim <= num = take idx subset Prelude.++ rep Prelude.++ drop lim subset
 | idx <= num = take idx subset Prelude.++ rep
 | otherwise = undefined where
 num = length subset; lim = idx + (length vals)
 rep = map Region vals
listedToCoin :: Int -> [[Boundary]] -> [Listed] -> [[Boundary]]
listedToCoin idx coins listeds
 | lim <= num = take idx coins Prelude.++ rep Prelude.++ drop lim coins
 | idx <= num = take idx coins Prelude.++ rep
 | otherwise = undefined where
 num = length coins; lim = idx + (length listeds)
 rep = map (\(Listed (ListedA1 _ vals)) -> map Boundary vals) listeds
planeToScalar :: Int -> Int -> [Plane] -> [Scalar]
planeToScalar idx siz planes = let
 num = length planes; lim = idx + siz
 sub = map (\x -> map GHC.double2Float (Matrix.toList x)) (drop num (take lim planes))
 in map (\x -> Scalar (ScalarA1 x)) sub
halfToNested :: Int -> Int -> Space -> [Nested]
halfToNested idx siz space = let
 num = length space; lim = idx + siz
 sub = map (\z -> map (\y -> map (\(Region x) -> x) y) z) (drop num (take lim space))
 in map (\x -> Nested (NestedA1 (length x) (map (\y -> Listed (ListedA1 (length y) y)) x))) sub
subsetToInt :: Int -> Int -> [Region] -> [Int]
subsetToInt idx siz subset = let
 num = length subset; lim = idx + siz
 in map (\(Region x) -> x) (drop num (take lim subset))
coinToListed :: Int -> Int -> [[Boundary]] -> [Listed]
coinToListed idx siz coin = let
 num = length coin; lim = idx + siz
 sub = map (\y -> map (\(Boundary x) -> x) y) (drop num (take lim coin))
 in map (\x -> Listed (ListedA1 (length x) x)) sub
pointToScalar :: Point -> Scalar
pointToScalar point = Scalar (ScalarA1 (map GHC.double2Float (Matrix.toList point)))
facetToListed :: [Int] -> Listed
facetToListed x = Listed (ListedA1 (length x) x)
listedToFacet :: Listed -> [Int]
listedToFacet (Listed (ListedA1 _ vals)) = vals

