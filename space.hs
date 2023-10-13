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
mainG idx state = do
 change <- readChange idx
 str <- newIORef ""
 showChange change str
 val <- readIORef str
 putStrLn val
 mainGF idx state change
mainGF :: Int -> State -> Change -> IO ()
mainGF _ _ (Change (ChangeA1 Type.Emergs _ _ _) _) = return ()
mainGF idx state change = mainG idx (mainH (mainI state change) change)

mainH :: State -> Change -> State
mainH state (Change (ChangeA1 Type.Numerics Type.Towrite idx 0) (ChangeA5B7 [])) = state
mainH (State a b c d e f g) change@(Change (ChangeA1 Type.Numerics Type.Towrite idx siz) (ChangeA5B7 (s:t))) =
 mainH (State (Map.insert idx s a) b c d e f g) (mainHF change)
mainH state (Change (ChangeA1 Type.Symbolics Type.Towrite idx 0) (ChangeA5B5 [])) = state
mainH (State a b c d e f g) change@(Change (ChangeA1 Type.Symbolics Type.Towrite idx siz) (ChangeA5B5 (s:t))) =
 mainH (State a (Map.insert idx s b) c d e f g) (mainHF change)
mainH state (Change (ChangeA1 Type.Triangles Type.Towrite idx 0) (ChangeA5B6 [])) = state
mainH (State a b c d e f g) change@(Change (ChangeA1 Type.Triangles Type.Towrite idx siz) (ChangeA5B6 (s:t))) =
 mainH (State a b (Map.insert idx s c) d e f g) (mainHF change)
mainH state (Change (ChangeA1 Type.Tangents Type.Towrite idx 0) (ChangeA5B5 [])) = state
mainH (State a b c d e f g) change@(Change (ChangeA1 Type.Tangents Type.Towrite idx siz) (ChangeA5B5 (s:t))) =
 mainH (State a b c (Map.insert idx s d) e f g) (mainHF change)
mainH state (Change (ChangeA1 Type.Vertexes Type.Towrite idx 0) (ChangeA5B7 [])) = state
mainH (State a b c d e f g) change@(Change (ChangeA1 Type.Vertexes Type.Towrite idx siz) (ChangeA5B7 (s:t))) =
 mainH (State a b c d (Map.insert idx s e) f g) (mainHF change)
mainH state (Change (ChangeA1 Type.Vertices Type.Towrite idx 0) (ChangeA5B6 [])) = state
mainH (State a b c d e f g) change@(Change (ChangeA1 Type.Vertices Type.Towrite idx siz) (ChangeA5B6 (s:t))) =
 mainH (State a b c d e (Map.insert idx s f) g) (mainHF change)
mainH state (Change (ChangeA1 Type.Polytopes Type.Towrite idx 0) (ChangeA5B5 [])) = state
mainH (State a b c d e f g) change@(Change (ChangeA1 Type.Polytopes Type.Towrite idx siz) (ChangeA5B5 (s:t))) =
 mainH (State a b c d e f (Map.insert idx s g)) (mainHF change)
mainH state change = state
mainHF :: Change -> Change
mainHF (Change (ChangeA1 cfg tdo idx siz) (ChangeA5B7 (s:t))) =
 (Change (ChangeA1 cfg tdo (idx + 1) (siz - 1)) (ChangeA5B7 t))

mainI :: State -> Change -> State
mainI state (Change (ChangeA1 Type.Numerics Type.Towrite idx 0) (ChangeA5B7 [])) = state
mainI state change@(Change (ChangeA1 Type.Numerics Type.Towrite idx siz) (ChangeA5B7 (s:t))) =
 mainI (mainIF idx state) (mainHF change)
mainI state (Change (ChangeA1 Type.Symbolics Type.Towrite idx 0) (ChangeA5B7 [])) = state
mainI state change@(Change (ChangeA1 Type.Symbolics Type.Towrite idx siz) (ChangeA5B7 (s:t))) =
 mainI (mainIF idx state) (mainHF change)
mainI state (Change (ChangeA1 Type.Triangles Type.Towrite idx 0) (ChangeA5B7 [])) = state
mainI state change@(Change (ChangeA1 Type.Triangles Type.Towrite idx siz) (ChangeA5B7 (s:t))) =
 mainI (mainIF idx state) (mainHF change)
mainI state (Change (ChangeA1 Type.Tangents Type.Towrite idx 0) (ChangeA5B7 [])) = state
mainI state change@(Change (ChangeA1 Type.Tangents Type.Towrite idx siz) (ChangeA5B7 (s:t))) =
 mainI (mainIF idx state) (mainHF change)
mainI state (Change (ChangeA1 Type.Vertexes Type.Towrite idx 0) (ChangeA5B7 [])) = state
mainI state change@(Change (ChangeA1 Type.Vertexes Type.Towrite idx siz) (ChangeA5B7 (s:t))) =
 mainI (mainIG idx state) (mainHF change)
mainI state (Change (ChangeA1 Type.Polytopes Type.Towrite idx 0) (ChangeA5B7 [])) = state
mainI state change@(Change (ChangeA1 Type.Polytopes Type.Towrite idx siz) (ChangeA5B7 (s:t))) =
 mainI (mainIH idx state) (mainHF change)
mainI state change = state
mainIF :: Int -> State -> State
mainIF idx (State a b c d e f g) = -- TODO remove aliased regions
 (State (Map.delete idx a) (Map.delete idx b) (Map.delete idx c) (Map.delete idx d) (mainIJ (Map.lookup idx d) e) (mainIJ (Map.lookup idx d) f) g)
mainIG :: Int -> State -> State
mainIG idx (State a b c d e f g) = -- TODO filter vertex from each of its tangents
 (State a b c d (Map.delete idx e) (Map.delete idx f) g)
mainIH :: Int -> State -> State
mainIH idx (State a b c d e f g) = -- TODO filter vertex from each of its tangents
 (State a b c d e f (Map.delete idx g))

mainIJ :: (Maybe Nested) -> (IntMap a) -> (IntMap a)
mainIJ Nothing a = a
mainIJ (Just (Nested (NestedA1 0 []))) a = a
mainIJ (Just (Nested (NestedA1 siz (s:t)))) a = mainIJ (Just (Nested (NestedA1 (siz - 1) t))) (Map.delete s a)
