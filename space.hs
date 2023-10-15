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
mainH ifd state (Change (ChangeA1 _ _ _ 0) _) = mainG ifd state
mainH ifd state (Change (ChangeA1 tag@Type.Numerics act@Type.Towrite idx siz) (ChangeA5B8 (val:lst))) = let
 nxt = Change (ChangeA1 tag act (idx + 1) (siz - 1)) (ChangeA5B8 lst)
 clr = mainI idx state
 (State a b c d e f g) = clr
 set = State (Map.insert idx val a) b c d e f g
 in mainH ifd set nxt
mainH ifd state (Change (ChangeA1 tag@Type.Symbolics act@Type.Towrite idx siz) (ChangeA5B5 (val:lst))) = let
 nxt = Change (ChangeA1 tag act (idx + 1) (siz - 1)) (ChangeA5B5 lst)
 clr = mainI idx state
 (State a b c d e f g) = clr
 set = State a (Map.insert idx val b) c d e f g
 in mainH ifd set nxt
mainH ifd state (Change (ChangeA1 tag@Type.Triangles act@Type.Towrite idx siz) (ChangeA5B7 (val:lst))) = let
 nxt = Change (ChangeA1 tag act (idx + 1) (siz - 1)) (ChangeA5B7 lst)
 clr = mainI idx state
 (State a b c d e f g) = clr
 set = State a b (Map.insert idx val c) d e f g
 in mainH ifd set nxt
mainH ifd state (Change (ChangeA1 tag@Type.Tangents act@Type.Towrite idx siz) (ChangeA5B6 (val:lst))) = let
 nxt = Change (ChangeA1 tag act (idx + 1) (siz - 1)) (ChangeA5B6 lst)
 clr = mainI idx state
 (State a b c d e f g) = clr
 set = State a b c (Map.insert idx val d) e f g
 in mainH ifd set nxt
mainH ifd state (Change (ChangeA1 tag@Type.Vertexes act@Type.Towrite idx siz) (ChangeA5B8 (val:lst))) = let
 nxt = Change (ChangeA1 tag act (idx + 1) (siz - 1)) (ChangeA5B8 lst)
 clr = mainJ idx state
 (State a b c d e f g) = clr
 set = State a b c d (Map.insert idx val e) f g
 in mainH ifd set nxt
mainH ifd state (Change (ChangeA1 tag@Type.Vertices act@Type.Towrite idx siz) (ChangeA5B7 (val:lst))) = let
 nxt = Change (ChangeA1 tag act (idx + 1) (siz - 1)) (ChangeA5B7 lst)
 clr = mainJ idx state
 (State a b c d e f g) = clr
 set = State a b c d e (Map.insert idx val f) g
 in mainH ifd set nxt
mainH ifd state (Change (ChangeA1 tag@Type.Polytopes act@Type.Towrite idx siz) (ChangeA5B6 (val:lst))) = let
 nxt = Change (ChangeA1 tag act (idx + 1) (siz - 1)) (ChangeA5B6 lst)
 clr = mainK idx state
 (State a b c d e f g) = clr
 set = State a b c d e f (Map.insert idx val g)
 in mainH ifd set nxt

mainI :: Int -> State -> State
mainI idx (State a b c d e f g) = let
 boundary = Boundary idx
 place = mainIG b
 space = placeToSpace place
 location = Boundary (locate boundary (boundariesOfPlace place))
 section = sectionSpace boundary place
 attached = takeRegions section place
 onside = Prelude.filter (\x -> sideToBool (regionWrtBoundary location x space)) attached
 numerics = Map.delete idx a
 symbolics = mainIJ (Map.delete idx b) onside
 triangles = Map.delete idx c
 tangents = Map.delete idx d
 lst = Map.lookup idx d
 vertexes = mainIF lst e
 vertices = mainIF lst f
 polytopes = mainII g onside
 in State numerics symbolics triangles tangents vertexes vertices polytopes
mainJ :: Int -> State -> State
mainJ idx (State a b c d e f g) = State a b c (mainJF idx d) (Map.delete idx e) (Map.delete idx f) g
mainK :: Int -> State -> State
mainK idx (State a b c d e f g) = let
 in State a b c d e f (Map.delete idx g)

mainIF :: (Maybe Listed) -> (IntMap a) -> (IntMap a)
mainIF Nothing a = a
mainIF (Just (Listed (ListedA1 0 []))) a = a
mainIF (Just (Listed (ListedA1 siz (s:t)))) a = mainIF (Just (Listed (ListedA1 (siz - 1) t))) (Map.delete s a)

mainIG :: (IntMap Nested) -> Place
mainIG nested = Prelude.map (\(idx,(Nested (NestedA1 _ listed))) -> (Boundary idx, Prelude.map mainIGF listed)) (Map.toList nested)
mainIGF :: Listed -> [Region]
mainIGF (Listed (ListedA1 _ lst)) = Prelude.map Region lst

mainIH :: Place -> (IntMap Nested)
mainIH place = Map.fromList (mainIHF place)
mainIHF :: Place -> [(Int,Nested)]
mainIHF place = Prelude.map mainIHG place
mainIHG :: (Boundary,[[Region]]) -> (Int,Nested)
mainIHG (boundary,full) = let
 lst = Prelude.map mainIHH full
 (Boundary idx) = boundary
 in (idx,(Nested (NestedA1 (length lst) lst)))
mainIHH :: [Region] -> Listed
mainIHH half = let
 lst = Prelude.map (\(Region x) -> x) half
 in Listed (ListedA1 (length lst) lst)

mainII :: (IntMap Listed) -> [Region] -> (IntMap Listed)
mainII listed onside = Map.map (mainIK onside) listed
mainIJ :: (IntMap Nested) -> [Region] -> (IntMap Nested)
mainIJ nested onside = Map.map (\(Nested (NestedA1 siz listed)) -> Nested (NestedA1 siz (Prelude.map (mainIK onside) listed))) nested
mainIK :: [Region] -> Listed -> Listed
mainIK onside (Listed (ListedA1 _ listed)) = let
 lst = Prelude.filter (\x -> elem (Region x) onside) listed
 in Listed (ListedA1 (length lst) lst)

mainJF :: Int -> (IntMap Listed) -> (IntMap Listed)
mainJF idx listed = Map.map (\(Listed (ListedA1 siz lst)) -> Listed (ListedA1 siz (Prelude.filter (\x -> x /= idx) lst))) listed
