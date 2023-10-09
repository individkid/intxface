module Main where

import Naive
import Face
import Type
import System.Environment
import Data.IORef
import Data.Maybe
import Data.IntMap

data State = State
 (IntMap Scalar) -- boundary -> plane
 (IntMap Scalar) -- vertex -> point
 (IntMap Triplet) -- vertex -> tangents
 (IntMap Triplet) -- boundary -> corners
 (IntMap Nested) -- boundary -> halfspaces
 (IntMap Nested) -- polytope -> regions

main :: IO ()
main = getArgs >>= mainF

mainF :: [String] -> IO ()
mainF [a] = do
 idx <- wrapIdent Type.Spacez a
 mainG (fromJust idx) (State empty empty empty empty empty empty)
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
mainH state (Change (ChangeA1 Type.Numerics Type.Towrite _ 0) (ChangeA5B7 [])) = return state
mainH (State a b c d e f) (Change (ChangeA1 Type.Numerics Type.Towrite idx siz) (ChangeA5B7 (h:t))) =
 mainH (State (insert idx h a) b c d e f) (Change (ChangeA1 Type.Numerics Type.Towrite (idx + 1) (siz - 1)) (ChangeA5B7 t))
mainH state (Change (ChangeA1 Type.Vertexes Type.Towrite _ 0) (ChangeA5B7 [])) = return state
mainH (State a b c d e f) (Change (ChangeA1 Type.Vertexes Type.Towrite idx siz) (ChangeA5B7 (h:t))) =
 mainH (State a (insert idx h b) c d e f) (Change (ChangeA1 Type.Vertexes Type.Towrite (idx + 1) (siz - 1)) (ChangeA5B7 t))
mainH state (Change (ChangeA1 Type.Vertices Type.Towrite _ 0) (ChangeA5B6 [])) = return state
mainH (State a b c d e f) (Change (ChangeA1 Type.Vertices Type.Towrite idx siz) (ChangeA5B6 (h:t))) =
 mainH (State a b (insert idx h c) d e f) (Change (ChangeA1 Type.Vertices Type.Towrite (idx + 1) (siz - 1)) (ChangeA5B6 t))
mainH state (Change (ChangeA1 Type.Triangles Type.Towrite _ 0) (ChangeA5B6 [])) = return state
mainH (State a b c d e f) (Change (ChangeA1 Type.Triangles Type.Towrite idx siz) (ChangeA5B6 (h:t))) =
 mainH (State a b c (insert idx h d) e f) (Change (ChangeA1 Type.Triangles Type.Towrite (idx + 1) (siz - 1)) (ChangeA5B6 t))
mainH state (Change (ChangeA1 Type.Boundaries Type.Towrite _ 0) (ChangeA5B6 [])) = return state
mainH (State a b c d e f) (Change (ChangeA1 Type.Boundaries Type.Towrite idx siz) (ChangeA5B5 (h:t))) =
 mainH (State a b c d (insert idx h e) f) (Change (ChangeA1 Type.Boundaries Type.Towrite (idx + 1) (siz - 1)) (ChangeA5B5 t))
mainH state (Change (ChangeA1 Type.Regions Type.Towrite _ 0) (ChangeA5B6 [])) = return state
mainH (State a b c d e f) (Change (ChangeA1 Type.Regions Type.Towrite idx siz) (ChangeA5B5 (h:t))) =
 mainH (State a b c d e (insert idx h f)) (Change (ChangeA1 Type.Regions Type.Towrite (idx + 1) (siz - 1)) (ChangeA5B5 t))
mainH state change = do
 str <- newIORef ""
 showChange change str
 val <- readIORef str
 putStrLn val
 return state

