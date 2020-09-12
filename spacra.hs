--
--    spacra.hs
--
--    This program is free software: you can redistribute it and/or modify
--    it under the terms of the GNU General Public License as published by
--    the Free Software Foundation, either version 3 of the License, or
--    (at your option) any later version.
--
--    This program is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU General Public License for more details.
--
--    You should have received a copy of the GNU General Public License
--    along with this program.  If not, see <http://www.gnu.org/licenses/>.
--

module Main where

import Naive
import Test.QuickCheck
import Test.QuickCheck.Test
import System.Exit
import Data.Bits
import Data.List

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
prop_subsets = forAll (prop_subsetsH 10) $
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
prop_holes = forAll (prop_subsetsH 10) $
 \a -> forAll (prop_subsetsH 10) $
 \b -> forAll (prop_subsetsG a) $
 \c -> let
 d = nub' (map abs c)
 e = holes b d
 f = d Naive.++ e
 in (prop_holesF d e) &&
 ((length e) == b) &&
 ((length f) == (b + (length d))) &&
 (((indices b) Naive.\\ f) == [])

putstr_simplex :: IO ()
putstr_simplex = let
 n = 4
 nindexs = indices (n + 1)
 nbounds = map Boundary nindexs
 nplace = powerSpace nbounds
 nspace = placeToSpace nplace
 nregions = regionsOfPlace nplace
 tindexs = indices (shift 1 (n + 1))
 tregions = map Region tindexs
 r = Region 8
 rplace = degenSpace r nplace
 rspace = placeToSpace rplace
 rindex = elemIndex' r tregions
 rregions = unplace rindex tregions
 one = 1 :: Int
 in do
 putStrLn (show (sort (regionsOfPlace rplace)))
 putStrLn (show rregions)
 putStrLn (show ((sort (regionsOfPlace rplace)) == rregions))
 -- ((regionsOfPlace rplace) == rregions) &&
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
 ((sort (regionsOfSpace nspace)) == tregions)

mainF :: Result -> IO ()
mainF a
 | isSuccess a = return ()
 | otherwise = exitFailure
main = do
 quickCheckResult prop_boolToSide >>= mainF
 quickCheckResult prop_sideToBool >>= mainF
 quickCheckResult prop_subsets >>= mainF
 quickCheckResult prop_holes >>= mainF
 -- putstr_simplex
 quickCheckResult prop_simplex >>= mainF
