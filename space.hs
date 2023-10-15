module Main where

import Naive
import Face
import Type
import System.Environment
import Data.IORef
import Data.Maybe
import Data.IntMap as Map

data State = State
 (IntMap Scalar) -- boundary -> plane -- Numerics
 (IntMap Nested) -- boundary -> onsides -- Symbolics
 (IntMap Triplet) -- boundary -> corners -- Triangles
 (IntMap Listed) -- boundary -> tangents -- Tangents
 (IntMap Scalar) -- vertex -> point -- Vertexes
 (IntMap Triplet) -- vertex -> tangents -- Vertices
 (IntMap Listed) -- polytope -> insides -- Polytopes

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
mainH ifd state (Change (ChangeA1 tag@Type.Numerics act@Type.Towrite idx siz) (ChangeA5B8 (val:lst))) = let
 nxt = Change (ChangeA1 tag act (idx + 1) (siz - 1)) (ChangeA5B8 lst)
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
mainH ifd state (Change (ChangeA1 tag@Type.Triangles act@Type.Towrite idx siz) (ChangeA5B7 (val:lst))) = let
 nxt = Change (ChangeA1 tag act (idx + 1) (siz - 1)) (ChangeA5B7 lst)
 clr = writeI idx state
 (State a b c d e f g) = clr
 set = State a b (Map.insert idx val c) d e f g
 in mainH ifd set nxt
mainH ifd state (Change (ChangeA1 tag@Type.Tangents act@Type.Towrite idx siz) (ChangeA5B6 (val:lst))) = let
 nxt = Change (ChangeA1 tag act (idx + 1) (siz - 1)) (ChangeA5B6 lst)
 clr = writeI idx state
 (State a b c d e f g) = clr
 set = State a b c (Map.insert idx val d) e f g
 in mainH ifd set nxt
mainH ifd state (Change (ChangeA1 tag@Type.Vertexes act@Type.Towrite idx siz) (ChangeA5B8 (val:lst))) = let
 nxt = Change (ChangeA1 tag act (idx + 1) (siz - 1)) (ChangeA5B8 lst)
 clr = writeJ idx state
 (State a b c d e f g) = clr
 set = State a b c d (Map.insert idx val e) f g
 in mainH ifd set nxt
mainH ifd state (Change (ChangeA1 tag@Type.Vertices act@Type.Towrite idx siz) (ChangeA5B7 (val:lst))) = let
 nxt = Change (ChangeA1 tag act (idx + 1) (siz - 1)) (ChangeA5B7 lst)
 clr = writeJ idx state
 (State a b c d e f g) = clr
 set = State a b c d e (Map.insert idx val f) g
 in mainH ifd set nxt
mainH ifd state (Change (ChangeA1 tag@Type.Polytopes act@Type.Towrite idx siz) (ChangeA5B6 (val:lst))) = let
 nxt = Change (ChangeA1 tag act (idx + 1) (siz - 1)) (ChangeA5B6 lst)
 clr = writeK idx state
 (State a b c d e f g) = clr
 set = State a b c d e f (Map.insert idx val g)
 in mainH ifd set nxt
mainH ifd state (Change (ChangeA1 tag@Type.Numerics Type.Toread idx siz) _) = let
 set = readI idx siz state
 (State a b c d e f g) = set
 slice = take siz (drop (idx - 1) (Map.elems a))
 resp = Change (ChangeA1 tag Type.Toresp idx siz) (ChangeA5B8 slice)
 in writeChange resp ifd >> mainG ifd set
mainH ifd state (Change (ChangeA1 tag@Type.Symbolics Type.Toread idx siz) _) = let
 set = readJ idx siz state
 (State a b c d e f g) = set
 slice = take siz (drop (idx - 1) (Map.elems b))
 resp = Change (ChangeA1 tag Type.Toresp idx siz) (ChangeA5B5 slice)
 in writeChange resp ifd >> mainG ifd set
mainH ifd state (Change (ChangeA1 tag@Type.Triangles Type.Toread idx siz) _) = let
 set = readK idx siz state
 (State a b c d e f g) = set
 slice = take siz (drop (idx - 1) (Map.elems c))
 resp = Change (ChangeA1 tag Type.Toresp idx siz) (ChangeA5B7 slice)
 in writeChange resp ifd >> mainG ifd set
mainH ifd state (Change (ChangeA1 tag@Type.Tangents Type.Toread idx siz) _) = let
 set = readL idx siz state
 (State a b c d e f g) = set
 slice = take siz (drop (idx - 1) (Map.elems d))
 resp = Change (ChangeA1 tag Type.Toresp idx siz) (ChangeA5B6 slice)
 in writeChange resp ifd >> mainG ifd set
mainH ifd state (Change (ChangeA1 tag@Type.Vertexes Type.Toread idx siz) _) = let
 set = readM idx siz state
 (State a b c d e f g) = set
 slice = take siz (drop (idx - 1) (Map.elems e))
 resp = Change (ChangeA1 tag Type.Toresp idx siz) (ChangeA5B8 slice)
 in writeChange resp ifd >> mainG ifd set
mainH ifd state (Change (ChangeA1 tag@Type.Vertices Type.Toread idx siz) _) = let
 set = readN idx siz state
 (State a b c d e f g) = set
 slice = take siz (drop (idx - 1) (Map.elems f))
 resp = Change (ChangeA1 tag Type.Toresp idx siz) (ChangeA5B7 slice)
 in writeChange resp ifd >> mainG ifd set
mainH ifd state (Change (ChangeA1 tag@Type.Polytopes Type.Toread idx siz) _) = let
 set = readO idx siz state
 (State a b c d e f g) = set
 slice = take siz (drop (idx - 1) (Map.elems g))
 resp = Change (ChangeA1 tag Type.Toresp idx siz) (ChangeA5B6 slice)
 in writeChange resp ifd >> mainG ifd set

readI :: Int -> Int -> State -> State
readI = undefined -- sample Symbolics to Numerics
readJ :: Int -> Int -> State -> State
readJ = undefined -- classify Numerics to Symbolics
readK :: Int -> Int -> State -> State
readK = undefined -- call readJ, and prepare Triangles from Polytopes
readL :: Int -> Int -> State -> State
readL = undefined -- permute domain of Numerics union Symbolics to Tangents
readM :: Int -> Int -> State -> State
readM = undefined -- call readL and reeadI, and intersect Numerics to Vertexes
readN :: Int -> Int -> State -> State
readN = undefined -- call readL, and backlink Tangents to Vertices
readO :: Int -> Int -> State -> State
readO = undefined -- fill in unwritten Polytopes with empties

writeI :: Int -> State -> State
writeI idx (State a b c d e f g) = let
 boundary = Boundary idx
 place = writeIG b
 space = placeToSpace place
 location = Boundary (locate boundary (boundariesOfPlace place))
 section = sectionSpace boundary place
 attached = takeRegions section place
 onside = Prelude.filter (\x -> sideToBool (regionWrtBoundary location x space)) attached
 numerics = Map.delete idx a
 symbolics = writeIJ (Map.delete idx b) onside
 triangles = Map.delete idx c
 tangents = Map.delete idx d
 lst = Map.lookup idx d
 vertexes = writeIF lst e
 vertices = writeIF lst f
 polytopes = writeII g onside
 in State numerics symbolics triangles tangents vertexes vertices polytopes
writeJ :: Int -> State -> State
writeJ idx (State a b c d e f g) = State a b c (writeJF idx d) (Map.delete idx e) (Map.delete idx f) g
writeK :: Int -> State -> State
writeK idx (State a b c d e f g) = let
 in State a b c d e f (Map.delete idx g)

writeIF :: (Maybe Listed) -> (IntMap a) -> (IntMap a)
writeIF Nothing a = a
writeIF (Just (Listed (ListedA1 0 []))) a = a
writeIF (Just (Listed (ListedA1 siz (s:t)))) a = writeIF (Just (Listed (ListedA1 (siz - 1) t))) (Map.delete s a)

writeIG :: (IntMap Nested) -> Place
writeIG nested = Prelude.map (\(idx,(Nested (NestedA1 _ listed))) -> (Boundary idx, Prelude.map writeIGF listed)) (Map.toList nested)
writeIGF :: Listed -> [Region]
writeIGF (Listed (ListedA1 _ lst)) = Prelude.map Region lst

writeIH :: Place -> (IntMap Nested)
writeIH place = Map.fromList (writeIHF place)
writeIHF :: Place -> [(Int,Nested)]
writeIHF place = Prelude.map writeIHG place
writeIHG :: (Boundary,[[Region]]) -> (Int,Nested)
writeIHG (boundary,full) = let
 lst = Prelude.map writeIHH full
 (Boundary idx) = boundary
 in (idx,(Nested (NestedA1 (length lst) lst)))
writeIHH :: [Region] -> Listed
writeIHH half = let
 lst = Prelude.map (\(Region x) -> x) half
 in Listed (ListedA1 (length lst) lst)

writeII :: (IntMap Listed) -> [Region] -> (IntMap Listed)
writeII listed onside = Map.map (writeIK onside) listed
writeIJ :: (IntMap Nested) -> [Region] -> (IntMap Nested)
writeIJ nested onside = Map.map (\(Nested (NestedA1 siz listed)) -> Nested (NestedA1 siz (Prelude.map (writeIK onside) listed))) nested
writeIK :: [Region] -> Listed -> Listed
writeIK onside (Listed (ListedA1 _ listed)) = let
 lst = Prelude.filter (\x -> elem (Region x) onside) listed
 in Listed (ListedA1 (length lst) lst)

writeJF :: Int -> (IntMap Listed) -> (IntMap Listed)
writeJF idx listed = Map.map (\(Listed (ListedA1 siz lst)) -> Listed (ListedA1 siz (Prelude.filter (\x -> x /= idx) lst))) listed
