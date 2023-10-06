module Main where

import Naive
import Face
import Type
import Test.QuickCheck
import Test.QuickCheck.Test
import System.Exit
import Data.Bits
import Data.List
import Data.Maybe
import Data.IORef
import System.Random
import Control.Exception

instance Arbitrary Side where
 arbitrary = frequency [(4, elements [Side 0]), (1, elements [Side 1])]

prop_boolToSide :: Bool -> Bool
prop_boolToSide a = sideToBool (boolToSide a) == a

prop_sideToBool :: Side -> Bool
prop_sideToBool a = boolToSide (sideToBool a) == a

prop_subsetsF :: [Int] -> [Int] -> Bool
prop_subsetsF a b = elem a (subsets (length a) b)
prop_subsetsG :: Int -> Gen [Int]
prop_subsetsG a = vector a
prop_subsetsH :: Int -> Gen Int
prop_subsetsH a = Test.QuickCheck.choose (0,a)
prop_subsetsI :: [Int] -> [Int] -> [Int]
prop_subsetsI [] a = a
prop_subsetsI _ [] = []
prop_subsetsI (a:b) c = let
 d = mod a (length c)
 e = take d c
 f = drop (d + 1) c
 g = e Prelude.++ f
 in prop_subsetsI b g
prop_subsets :: Property
prop_subsets = forAll (prop_subsetsH 20) $
 \a -> forAll (prop_subsetsH a) $
 \b -> forAll (prop_subsetsG a) $
 \c -> forAll (prop_subsetsG b) $
 \d -> prop_subsetsF (prop_subsetsI d c) c

prop_holesF :: [Int] -> [Int] -> Bool
prop_holesF (a:b:c) (d:e:f)
 | a < d = (a < b) && (prop_holesF (b:c) (d:e:f))
 | a > d = (d < e) && (prop_holesF (a:b:c) (e:f))
 | otherwise = False
prop_holesF [a] (b:c) = (a /= b) && (prop_holesF [] (b:c))
prop_holesF (a:b) [c] = (a /= c) && (prop_holesF (a:b) [])
prop_holesF [] (a:b:c) = (a < b) && (prop_holesF [] (b:c))
prop_holesF (a:b:c) [] = (a < b) && (prop_holesF (b:c) [])
prop_holesF _ _ = True
prop_holes :: Property
prop_holes = forAll (prop_subsetsH 20) $ \a ->
 forAll (prop_subsetsH 20) $ \b ->
 forAll (prop_subsetsG a) $ \c -> let
 d = nub' (map abs c)
 e = holes b d
 f = d Naive.++ e
 in (prop_holesF d e) &&
 ((length e) == b) &&
 ((length f) == (b + (length d))) &&
 (((indices b) Naive.\\ f) == [])

prop_simplex :: Property
prop_simplex =
 forAll (Test.QuickCheck.choose (2,4)) $ \n -> let
 nindexs = indices (n + 1)
 nbounds = map Boundary nindexs
 nplace = powerSpace nbounds
 nspace = placeToSpace nplace
 nregions = regionsOfPlace nplace
 tindexs = indices (shift 1 (n + 1))
 tregions = map Region tindexs
 in forAll (elements nregions) $ \r -> let
 rplace = degenSpace r nplace
 rspace = placeToSpace rplace
 rindex = elemIndex' r tregions
 rregions = unplace rindex tregions
 [mregion] = filter (\x -> not (outsideOfRegionExists x rspace)) rregions
 [tregion] = filter (\x -> migrateSpaceExists x rspace) rregions
 mspace = migrateSpace mregion rspace
 in (isLinear n rspace) &&
 (not (isLinear (n + 1) rspace)) &&
 (not (isLinear (n - 1) rspace)) &&
 ((boundariesOfPlace rplace) == nbounds) &&
 ((boundariesOfSpace rspace) == nbounds) &&
 ((sort (regionsOfPlace rplace)) == rregions) &&
 ((sort (regionsOfSpace rspace)) == rregions) &&
 (not (isLinear n nspace)) &&
 (isLinear (n + 1) nspace) &&
 (not (isLinear (n - 1) nspace)) &&
 ((boundariesOfPlace nplace) == nbounds) &&
 ((boundariesOfSpace nspace) == nbounds) &&
 ((sort (regionsOfPlace nplace)) == tregions) &&
 ((sort (regionsOfSpace nspace)) == tregions) &&
 (isLinear n mspace) &&
 (mspace /= rspace) &&
 (mregion == tregion)

prop_equiv :: Property
prop_equiv =
 forAll (Test.QuickCheck.choose (2,4)) $ \n -> let
 nindexs = indices (n + 1)
 nbounds = map Boundary nindexs
 nplace = powerSpace nbounds
 nspace = placeToSpace nplace
 nregions = regionsOfPlace nplace
 in forAll (elements nregions) $ \r -> let
 rplace = degenSpace r nplace
 rspace = placeToSpace rplace
 requiv = equivSpace rspace
 in (isLinear n requiv) &&
 ((compare rspace requiv) /= LT)

prop_generateF :: (RandomGen g) => (g -> (Double,g)) -> g -> Int -> ([Double],g)
prop_generateF = catalyze
prop_generate :: Property
prop_generate =
 forAll (Test.QuickCheck.choose (-100000,100000)) $ \a ->
 forAll (Test.QuickCheck.choose (0,10)) $ \b -> let
 g = mkStdGen a
 (c,h) = prop_generateF (\i -> System.Random.randomR (-100.0,100.0) i) g b
 in ((length c) == b) &&
 (all (\x -> (x >= -100.0) && (x <= 100.0)) c)

prop_subspace :: Property
prop_subspace =
 forAll (Test.QuickCheck.choose (2,4)) $ \n -> let
 nindexs = indices (n + 1)
 nbounds = map Boundary nindexs
 nplace = powerSpace nbounds
 nspace = placeToSpace nplace
 nregions = regionsOfPlace nplace
 tindexs = indices (shift 1 (n + 1))
 tregions = map Region tindexs
 in forAll (elements nregions) $ \r -> let
 rplace = degenSpace r nplace
 in forAll (elements nbounds) $ \b -> let
 bplace = subSpace b rplace
 in (isLinear n (placeToSpace bplace)) &&
 (isSubSpace bplace rplace)

prop_sectionspace :: Property
prop_sectionspace =
 forAll (Test.QuickCheck.choose (2,4)) $ \n -> let
 nindexs = indices (n + 1)
 nbounds = map Boundary nindexs
 nplace = powerSpace nbounds
 nspace = placeToSpace nplace
 nregions = regionsOfPlace nplace
 tindexs = indices (shift 1 (n + 1))
 tregions = map Region tindexs
 in forAll (elements nregions) $ \r -> let
 rplace = degenSpace r nplace
 in forAll (elements nbounds) $ \b -> let
 bplace = sectionSpace b rplace
 in isLinear (n - 1) (placeToSpace bplace) &&
 (isSectionSpace bplace rplace)

prop_space :: Property
prop_space =
 forAll (Test.QuickCheck.choose(2,3)) $ \n ->
 forAll (Test.QuickCheck.choose(0,8)) $ \m -> let
 s = anySpace n m
 in isLinear n s

putstr_linear :: IO ()
putstr_linear = let
 n = 3
 m = 4
 s = anySpace n m
 in 
 putStrLn (show (isLinear n s))

putstr_space :: IO ()
putstr_space = let
 n = 3
 m = 3
 bounds = boundaryHoles m []
 prefix = take (m - 1) bounds
 suffix = drop (m - 1) bounds
 place = foldl' (\y x -> superSpace n y (powerSpace [x])) [] prefix
 space = placeToSpace place
 power = powerSpace suffix
 share = powerSpace [Naive.choose (boundariesOfSpace space)]
 s = place
 t = superSpace n share power
 sBounds = boundariesOfPlace s
 tBounds = boundariesOfPlace t
 sOnly = sBounds Naive.\\ tBounds
 bound = Naive.choose sOnly
 sup = superSpace n (subSpace bound s) t
 u = superSpace n s sup
 in do
 putStrLn (show (isLinear n space))
 putStrLn (show (isLinear n (placeToSpace t)))
 putStrLn (show (isLinear n (placeToSpace u)))
 putStrLn (show (length (boundariesOfPlace u)))
 putStrLn (show (boundariesOfPlace u))
 putStrLn (show (anySpace n m))

cmpstr_hideF :: IORef String -> IO (Maybe Emerg) -- TODO only needed for depend.lua until targets depend on function names
cmpstr_hideF = hideEmerg
cmpstr_hide :: IO ()
cmpstr_hide = let
 idx = openPipe
 in do
 ref <- newIORef "Emerg(Numerics)123"
 may <- cmpstr_hideF ref
 str <- readIORef ref
 ior <- newIORef ""
 showEmerg (fromJust may) ior
 val <- readIORef ior
 assert (val == "Emerg(Numerics)") (return ())
 assert (str == "123") (return ())

mainF :: Result -> IO ()
mainF a
 | isSuccess a = return ()
 | otherwise = exitFailure
main :: IO ()
main = do
 cmpstr_hide
 quickCheckResult prop_boolToSide >>= mainF
 quickCheckResult prop_sideToBool >>= mainF
 quickCheckResult prop_subsets >>= mainF
 quickCheckResult prop_holes >>= mainF
 quickCheckResult prop_simplex >>= mainF
 quickCheckResult prop_equiv >>= mainF
 quickCheckResult prop_generate >>= mainF
 quickCheckResult prop_subspace >>= mainF
 quickCheckResult prop_sectionspace >>= mainF
 -- quickCheckResult prop_space >>= mainF
 -- putstr_space
