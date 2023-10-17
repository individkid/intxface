module Main where

import Naive
import Face
import Type
import System.Environment
import Data.IORef
import Data.Maybe
import Data.IntMap as Map

data State = State
 (IntMap Scalar) -- boundary -> scalar triplet -- Numerics -- calculated from Symbolics
 (IntMap Nested) -- boundary -> region list pair -- Symbolics -- calculated from Numerics
 (IntMap Listed) -- boundary -> vertex list -- Vertexes -- calculated from Symbolics
 (IntMap Scalar) -- vertex -> scalar triplet -- Vertices -- calculated from Symbolics
 (IntMap Listed) -- vertex -> boundary triplet -- Tangents -- calculated from Symbolics
 (IntMap Nested) -- polytope -> region list pair -- Polytopes -- calculated from Symbolics
 (IntMap Nested) -- polytope -> vertex triplet list -- Triangles -- calculated from Symbolics
-- ChangeA5B5 [Nested] | -- nst -- cfg:Polytopes,Symbolics,Tangents,Triangles;vld:Toresp,Towrite
-- ChangeA5B6 [Listed] | -- lst -- cfg:Vertexes;vld:Toresp,Towrite
-- ChangeA5B7 [Scalar] | -- vec -- cfg:Numerics,Vertices;vld:Toresp,Towrite

main :: IO ()
main = getArgs >>= mainF

mainF :: [String] -> IO ()
mainF [a] = do
 idx <- wrapIdent Type.Spacez a
 mainG (fromJust idx) (State Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty)
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
mainH ifd state (Change (ChangeA1 _ Type.Towrite _ 0) _) = mainG ifd state

mainH ifd state (Change (ChangeA1 tag@Type.Numerics act@Type.Towrite idx siz) (ChangeA5B7 (val:lst))) = let
 nxt = Change (ChangeA1 tag act (idx + 1) (siz - 1)) (ChangeA5B7 lst)
 clr = writeI idx state
 (State a b c d e f g) = clr
 set = State (Map.insert idx val a) b c d e f g
 in mainH ifd set nxt
mainH ifd state (Change (ChangeA1 tag@Type.Symbolics act@Type.Towrite idx siz) (ChangeA5B5 (val:lst))) = let
 nxt = Change (ChangeA1 tag act (idx + 1) (siz - 1)) (ChangeA5B5 lst)
 clr = writeI idx state
 (State a b c d e f g) = clr
 set = State a (Map.insert idx val b) c d e f g
 in mainH ifd set nxt
mainH ifd state (Change (ChangeA1 tag@Type.Vertexes act@Type.Towrite idx siz) (ChangeA5B6 (val:lst))) = let
 nxt = Change (ChangeA1 tag act (idx + 1) (siz - 1)) (ChangeA5B6 lst)
 clr = writeJ idx state
 (State a b c d e f g) = clr
 set = State a b (Map.insert idx val c) d e f g
 in mainH ifd set nxt
mainH ifd state (Change (ChangeA1 tag@Type.Vertices act@Type.Towrite idx siz) (ChangeA5B7 (val:lst))) = let
 nxt = Change (ChangeA1 tag act (idx + 1) (siz - 1)) (ChangeA5B7 lst)
 clr = writeJ idx state
 (State a b c d e f g) = clr
 set = State a b c (Map.insert idx val d) e f g
 in mainH ifd set nxt
mainH ifd state (Change (ChangeA1 tag@Type.Tangents act@Type.Towrite idx siz) (ChangeA5B6 (val:lst))) = let
 nxt = Change (ChangeA1 tag act (idx + 1) (siz - 1)) (ChangeA5B6 lst)
 clr = writeI idx state
 (State a b c d e f g) = clr
 set = State a b c d (Map.insert idx val e) f g
 in mainH ifd set nxt
mainH ifd state (Change (ChangeA1 tag@Type.Polytopes act@Type.Towrite idx siz) (ChangeA5B5 (val:lst))) = let
 nxt = Change (ChangeA1 tag act (idx + 1) (siz - 1)) (ChangeA5B5 lst)
 clr = writeK idx state
 (State a b c d e f g) = clr
 set = State a b c d e (Map.insert idx val f) g
 in mainH ifd set nxt
mainH ifd state (Change (ChangeA1 tag@Type.Triangles act@Type.Towrite idx siz) (ChangeA5B5 (val:lst))) = let
 nxt = Change (ChangeA1 tag act (idx + 1) (siz - 1)) (ChangeA5B5 lst)
 clr = writeI idx state
 (State a b c d e f g) = clr
 set = State a b c d e f (Map.insert idx val g)
 in mainH ifd set nxt

mainH ifd state (Change (ChangeA1 tag@Type.Numerics Type.Toread idx siz) _) = let
 set = readI idx siz state
 (State a b c d e f g) = set
 slice = take siz (drop (idx - 1) (Map.elems a))
 resp = Change (ChangeA1 tag Type.Toresp idx siz) (ChangeA5B7 slice)
 in writeChange resp ifd >> mainG ifd set
mainH ifd state (Change (ChangeA1 tag@Type.Symbolics Type.Toread idx siz) _) = let
 set = readJ idx siz state
 (State a b c d e f g) = set
 slice = take siz (drop (idx - 1) (Map.elems b))
 resp = Change (ChangeA1 tag Type.Toresp idx siz) (ChangeA5B5 slice)
 in writeChange resp ifd >> mainG ifd set
mainH ifd state (Change (ChangeA1 tag@Type.Vertexes Type.Toread idx siz) _) = let
 set = readK idx siz state
 (State a b c d e f g) = set
 slice = take siz (drop (idx - 1) (Map.elems c))
 resp = Change (ChangeA1 tag Type.Toresp idx siz) (ChangeA5B6 slice)
 in writeChange resp ifd >> mainG ifd set
mainH ifd state (Change (ChangeA1 tag@Type.Vertices Type.Toread idx siz) _) = let
 set = readL idx siz state
 (State a b c d e f g) = set
 slice = take siz (drop (idx - 1) (Map.elems d))
 resp = Change (ChangeA1 tag Type.Toresp idx siz) (ChangeA5B7 slice)
 in writeChange resp ifd >> mainG ifd set
mainH ifd state (Change (ChangeA1 tag@Type.Tangents Type.Toread idx siz) _) = let
 set = readM idx siz state
 (State a b c d e f g) = set
 slice = take siz (drop (idx - 1) (Map.elems e))
 resp = Change (ChangeA1 tag Type.Toresp idx siz) (ChangeA5B6 slice)
 in writeChange resp ifd >> mainG ifd set
mainH ifd state (Change (ChangeA1 tag@Type.Polytopes Type.Toread idx siz) _) = let
 set = readN idx siz state
 (State a b c d e f g) = set
 slice = take siz (drop (idx - 1) (Map.elems f))
 resp = Change (ChangeA1 tag Type.Toresp idx siz) (ChangeA5B5 slice)
 in writeChange resp ifd >> mainG ifd set
mainH ifd state (Change (ChangeA1 tag@Type.Triangles Type.Toread idx siz) _) = let
 set = readO idx siz state
 (State a b c d e f g) = set
 slice = take siz (drop (idx - 1) (Map.elems g))
 resp = Change (ChangeA1 tag Type.Toresp idx siz) (ChangeA5B5 slice)
 in writeChange resp ifd >> mainG ifd set

readI :: Int -> Int -> State -> State
readI = undefined
readJ :: Int -> Int -> State -> State
readJ = undefined
readK :: Int -> Int -> State -> State
readK = undefined
readL :: Int -> Int -> State -> State
readL = undefined
readM :: Int -> Int -> State -> State
readM = undefined
readN :: Int -> Int -> State -> State
readN = undefined
readO :: Int -> Int -> State -> State
readO = undefined

-- (IntMap Scalar) -- boundary -> scalar triplet -- Numerics -- calculated from Symbolics
-- (IntMap Nested) -- boundary -> region list pair -- Symbolics -- calculated from Numerics
-- (IntMap Listed) -- boundary -> vertex list -- Vertexes -- calculated from Symbolics
-- (IntMap Scalar) -- vertex -> scalar triplet -- Vertices -- calculated from Symbolics
-- (IntMap Listed) -- vertex -> boundary triplet -- Tangents -- calculated from Symbolics
-- (IntMap Nested) -- polytope -> region list pair -- Polytopes -- calculated from Symbolics
-- (IntMap Nested) -- polytope -> vertex triplet list -- Triangles -- calculated from Symbolics
writeI :: Int -> State -> State
writeI idx (State a b c d e f g) = let
 boundary = Boundary idx
 place = writeIG b
 regions = writeIF boundary place
 corners = writeII boundary e
 numerics = Map.delete idx a
 symbolics = writeIH (Map.delete idx b) regions
 vertexes = Map.delete idx c
 -- assume vertices and tangents have same domain
 vertices = writeIJ corners d
 tangents = writeIJ corners e
 -- assume polytope regions are in symbolic
 polytopes = writeIH (Map.delete idx f) regions 
 -- assume range vertices are in tangents domain
 triangles = writeIK corners g
 in State numerics symbolics vertexes vertices tangents polytopes triangles
writeIF :: Boundary -> Place -> [Region]
writeIF boundary place | elem boundary boundaries = let
 space = placeToSpace place
 location = Boundary (locate boundary boundaries)
 section = sectionSpace boundary place
 attached = takeRegions section place
 in Prelude.filter (\x -> sideToBool (regionWrtBoundary location x space)) attached where
 boundaries = boundariesOfPlace place
writeIF _ _ = []
writeIG :: (IntMap Nested) -> Place
writeIG nested = Prelude.map (\(idx,(Nested (NestedA1 _ listed))) -> (Boundary idx, Prelude.map writeIGF listed)) (Map.toList nested)
writeIGF :: Listed -> [Region]
writeIGF (Listed (ListedA1 _ lst)) = Prelude.map Region lst
writeIH :: (IntMap Nested) -> [Region] -> (IntMap Nested)
writeIH nested regions = Map.map (\(Nested (NestedA1 siz listed)) -> Nested (NestedA1 siz (Prelude.map (writeIHF regions) listed))) nested
writeIHF :: [Region] -> Listed -> Listed
writeIHF regions (Listed (ListedA1 siz lst)) = let
 intlst = Prelude.map (\x -> Region x) lst
 sublst = Prelude.filter (\x -> not (elem x regions)) intlst
 reglst = Prelude.map (\(Region x) -> x) sublst
 in Listed (ListedA1 (length reglst) reglst)
writeII :: Boundary -> (IntMap Listed) -> [Int]
writeII boundary tangents = let (Boundary y) = boundary in Map.keys (Map.filter (\(Listed (ListedA1 _ x)) -> elem y x) tangents)
writeIJ :: [Int] -> (IntMap a) -> (IntMap a)
writeIJ corners intmap = fromList (Prelude.filter (\(x,_) -> not (elem x corners)) (toList intmap))
writeIK :: [Int] -> (IntMap Nested) -> (IntMap Nested)
writeIK = undefined

writeJ :: Int -> State -> State
writeJ = undefined

writeK :: Int -> State -> State
writeK = undefined
