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

prop_tetrahedron :: Property
prop_tetrahedron = forAll (vector 4) $
 \a -> let
 bounds = map Boundary a
 power = powerSpace bounds
 regions = regionsOfPlace power
 in forAll (elements regions) $
 \b -> let
 degen = degenSpace b power
 space = placeToSpace degen
 in isLinear 3 space

mainF :: Result -> IO ()
mainF a
 | isSuccess a = return ()
 | otherwise = exitFailure
main = do
 quickCheckResult prop_boolToSide >>= mainF
 quickCheckResult prop_sideToBool >>= mainF
 quickCheckResult prop_subsets >>= mainF
 quickCheckResult prop_holes >>= mainF
 quickCheckResult prop_tetrahedron >>= mainF
