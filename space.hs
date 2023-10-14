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
 (IntMap Nested) -- boundary -> tangents -- Tangents
 (IntMap Scalar) -- vertex -> point -- Vertexes
 (IntMap Triplet) -- vertex -> tangents -- Vertices
 (IntMap Nested) -- polytope -> insides -- Polytopes
-- data ChangeA5 =
--     ChangeA5B5 [Nested] | -- lst -- cfg:Polytopes,Symbolics,Tangents
--     ChangeA5B6 [Triplet] | -- tri -- cfg:Triangles,Vertices
--     ChangeA5B7 [Scalar] | -- vec -- cfg:Numerics,Vertexes
--     ChangeA5Bs deriving (Eq)

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
mainH ifd state (Change (ChangeA1 tag@Type.Numerics act@Type.Towrite idx siz) (ChangeA5B7 (val:lst))) = let
 nxt = Change (ChangeA1 tag act (idx + 1) (siz - 1)) (ChangeA5B7 lst)
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
mainH ifd state (Change (ChangeA1 tag@Type.Triangles act@Type.Towrite idx siz) (ChangeA5B6 (val:lst))) = let
 nxt = Change (ChangeA1 tag act (idx + 1) (siz - 1)) (ChangeA5B6 lst)
 clr = mainI idx state
 (State a b c d e f g) = clr
 set = State a b (Map.insert idx val c) d e f g
 in mainH ifd set nxt
mainH ifd state (Change (ChangeA1 tag@Type.Tangents act@Type.Towrite idx siz) (ChangeA5B5 (val:lst))) = let
 nxt = Change (ChangeA1 tag act (idx + 1) (siz - 1)) (ChangeA5B5 lst)
 clr = mainI idx state
 (State a b c d e f g) = clr
 set = State a b c (Map.insert idx val d) e f g
 in mainH ifd set nxt
mainH ifd state (Change (ChangeA1 tag@Type.Vertexes act@Type.Towrite idx siz) (ChangeA5B7 (val:lst))) = let
 nxt = Change (ChangeA1 tag act (idx + 1) (siz - 1)) (ChangeA5B7 lst)
 clr = mainJ idx state
 (State a b c d e f g) = clr
 set = State a b c d (Map.insert idx val e) f g
 in mainH ifd set nxt
mainH ifd state (Change (ChangeA1 tag@Type.Vertices act@Type.Towrite idx siz) (ChangeA5B6 (val:lst))) = let
 nxt = Change (ChangeA1 tag act (idx + 1) (siz - 1)) (ChangeA5B6 lst)
 clr = mainJ idx state
 (State a b c d e f g) = clr
 set = State a b c d e (Map.insert idx val f) g
 in mainH ifd set nxt
mainH ifd state (Change (ChangeA1 tag@Type.Polytopes act@Type.Towrite idx siz) (ChangeA5B5 (val:lst))) = let
 nxt = Change (ChangeA1 tag act (idx + 1) (siz - 1)) (ChangeA5B5 lst)
 clr = mainK idx state
 (State a b c d e f g) = clr
 set = State a b c d e f (Map.insert idx val g)
 in mainH ifd set nxt

mainI :: Int -> State -> State
mainI idx (State a b c d e f g) = -- TODO remove aliased regions
 (State (Map.delete idx a) (Map.delete idx b) (Map.delete idx c) (Map.delete idx d) (mainL (Map.lookup idx d) e) (mainL (Map.lookup idx d) f) g)
mainJ :: Int -> State -> State
mainJ idx (State a b c d e f g) = -- TODO filter vertex from each of its tangents
 (State a b c d (Map.delete idx e) (Map.delete idx f) g)
mainK :: Int -> State -> State
mainK idx (State a b c d e f g) =
 (State a b c d e f (Map.delete idx g))

mainL :: (Maybe Nested) -> (IntMap a) -> (IntMap a)
mainL Nothing a = a
mainL (Just (Nested (NestedA1 0 []))) a = a
mainL (Just (Nested (NestedA1 siz (s:t)))) a = mainL (Just (Nested (NestedA1 (siz - 1) t))) (Map.delete s a)



