-- "Origami Squares" (c) by Ignacio Slater M.

-- "Origami Squares" is licensed under a
-- Creative Commons Attribution 4.0 International License.

-- You should have received a copy of the license along with this
-- work. If not, see <http://creativecommons.org/licenses/by/4.0/>.

data Nat = Zero | Succ Nat deriving (Eq, Ord, Show)

add :: Nat -> Nat -> Nat
add Zero     m = m
add (Succ n) m = Succ (add n m)

mult :: Nat -> Nat -> Nat
mult Zero     _ = Zero
mult (Succ n) m = add m (mult n m)

foldNat :: (b -> b) -> b -> Nat -> b
foldNat _ v Zero     = v
foldNat f v (Succ n) = f (foldNat f v n)

{-|
Sum of squares from 0 to n
-}
sumsqr :: Nat -> Nat
sumsqr = snd . foldNat f v 
  where
    v = (Zero, Zero)
    f (n, acc) = (Succ n, add acc (mult (Succ n) (Succ n)))
