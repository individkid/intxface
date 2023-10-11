module Main where

import Naive
import Face
import Type
import System.Environment
import Data.IORef
import Data.Maybe
import Data.IntMap as Map
import Data.IntSet as Set

data State = State
 (IntMap Scalar) -- boundary -> plane -- Numerics
 (IntMap Nested) -- boundary -> onsides -- Symbolics
 (IntMap Triplet) -- boundary -> corners -- Triangles
 (IntMap Nested) -- boundary -> tangents -- Tangents
 (IntMap Scalar) -- vertex -> point -- Vertexes
 (IntMap Triplet) -- vertex -> tangents -- Vertices
 (IntMap Nested) -- polytope -> insides -- Polytopes
 (IntSet) -- regions -- Regions
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
 mainG (fromJust idx) (State Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty Set.empty)
 return ()
mainF _ = undefined

mainG :: Int -> State -> IO State
mainG idx state = do
 change <- readChange idx
 if (getChangeCcfg change == Type.Emergs) then
  (return state)
 else
  (mainH state change >>= mainG idx)

mainH :: State -> Change -> IO State
mainH state (Change (ChangeA1 Type.Numerics Type.Towrite idx 0) (ChangeA5B7 [])) = mainHA idx state
mainH (State a b c d e f g h) (Change (ChangeA1 Type.Numerics Type.Towrite idx siz) (ChangeA5B7 (s:t))) =
 mainH (State (Map.insert idx s a) b c d e f g h) (Change (ChangeA1 Type.Numerics Type.Towrite (idx + 1) (siz - 1)) (ChangeA5B7 t))
mainH state (Change (ChangeA1 Type.Symbolics Type.Towrite idx 0) (ChangeA5B5 [])) = mainHB idx state
mainH (State a b c d e f g h) (Change (ChangeA1 Type.Symbolics Type.Towrite idx siz) (ChangeA5B5 (s:t))) =
 mainH (State a (Map.insert idx s b) c d e f g h) (Change (ChangeA1 Type.Symbolics Type.Towrite (idx + 1) (siz - 1)) (ChangeA5B5 t))
mainH state (Change (ChangeA1 Type.Triangles Type.Towrite idx 0) (ChangeA5B6 [])) = mainHC idx state
mainH (State a b c d e f g h) (Change (ChangeA1 Type.Triangles Type.Towrite idx siz) (ChangeA5B6 (s:t))) =
 mainH (State a b (Map.insert idx s c) d e f g h) (Change (ChangeA1 Type.Triangles Type.Towrite (idx + 1) (siz - 1)) (ChangeA5B6 t))
mainH state (Change (ChangeA1 Type.Tangents Type.Towrite idx 0) (ChangeA5B5 [])) = mainHD idx state
mainH (State a b c d e f g h) (Change (ChangeA1 Type.Tangents Type.Towrite idx siz) (ChangeA5B5 (s:t))) =
 mainH (State a b c (Map.insert idx s d) e f g h) (Change (ChangeA1 Type.Tangents Type.Towrite (idx + 1) (siz - 1)) (ChangeA5B5 t))
mainH state (Change (ChangeA1 Type.Vertexes Type.Towrite idx 0) (ChangeA5B7 [])) = mainHE idx state
mainH (State a b c d e f g h) (Change (ChangeA1 Type.Vertexes Type.Towrite idx siz) (ChangeA5B7 (s:t))) =
 mainH (State a b c d (Map.insert idx s e) f g h) (Change (ChangeA1 Type.Vertexes Type.Towrite (idx + 1) (siz - 1)) (ChangeA5B7 t))
mainH state (Change (ChangeA1 Type.Vertices Type.Towrite idx 0) (ChangeA5B6 [])) = mainHF idx state
mainH (State a b c d e f g h) (Change (ChangeA1 Type.Vertices Type.Towrite idx siz) (ChangeA5B6 (s:t))) =
 mainH (State a b c d e (Map.insert idx s f) g h) (Change (ChangeA1 Type.Vertices Type.Towrite (idx + 1) (siz - 1)) (ChangeA5B6 t))
mainH state (Change (ChangeA1 Type.Polytopes Type.Towrite idx 0) (ChangeA5B5 [])) = mainHG idx state
mainH (State a b c d e f g h) (Change (ChangeA1 Type.Polytopes Type.Towrite idx siz) (ChangeA5B5 (s:t))) =
 mainH (State a b c d e f (Map.insert idx s g) h) (Change (ChangeA1 Type.Polytopes Type.Towrite (idx + 1) (siz - 1)) (ChangeA5B5 t))
mainH state (Change (ChangeA1 Type.Regions Type.Towrite idx 0) ChangeA5Bs) = mainHH idx state
mainH (State a b c d e f g h) (Change (ChangeA1 Type.Regions Type.Towrite idx siz) ChangeA5Bs) =
 mainH (State a b c d e f g (Set.union h (Set.fromList [idx..(idx + siz - 1)]))) (Change (ChangeA1 Type.Regions Type.Towrite (idx + 1) (siz - 1)) ChangeA5Bs)
mainH state change = do
 str <- newIORef ""
 showChange change str
 val <- readIORef str
 putStrLn val
 return state
mainHA :: Int -> State -> IO State
mainHA idx (State a b c d e f g h) =
 return (State a (Map.delete idx b) (Map.delete idx c) (Map.delete idx d) (mainI (Map.lookup idx d) e) (mainI (Map.lookup idx d) f) g h)
mainHB :: Int -> State -> IO State
mainHB = undefined
mainHC :: Int -> State -> IO State
mainHC = undefined
mainHD :: Int -> State -> IO State
mainHD = undefined
mainHE :: Int -> State -> IO State
mainHE = undefined
mainHF :: Int -> State -> IO State
mainHF = undefined
mainHG :: Int -> State -> IO State
mainHG = undefined
mainHH :: Int -> State -> IO State
mainHH = undefined

mainI :: (Maybe Nested) -> (IntMap a) -> (IntMap a)
mainI Nothing a = a
mainI (Just (Nested (NestedA1 0 []))) a = a
mainI (Just (Nested (NestedA1 siz (s:t)))) a = mainI (Just (Nested (NestedA1 (siz - 1) t))) (Map.delete s a)
