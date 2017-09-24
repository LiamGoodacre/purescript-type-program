module Data.CCC
  ( module Control.Semigroupoid
  , module Control.Category
  , module Data.Semigroup
  , module Data.Monoid
  , class Cartesian
  , fork
  , exl
  , exr
  , class Closed
  , apply
  , curry
  , uncurry
  , class Terminal
  , it
  , class Constant
  , constant
  , class HeytingCategory
  , notC
  , disjC
  , conjC
  , impliesC
  , ttC
  , ffC
  , class SemigroupCategory
  , appendC
  , class MonoidCategory
  , memptyC
  ) where

import Control.Semigroupoid (class Semigroupoid, compose)
import Control.Category (class Category, id)
import Prelude (Unit, unit)
import Data.HeytingAlgebra (implies, tt, ff, not, disj, conj, class HeytingAlgebra)
import Data.Tuple (Tuple(..))
import Data.Semigroup (class Semigroup, append)
import Data.Monoid (class Monoid, mempty)

-- term level type classes for cartesion, closed, and heyting categories

class Category k <= Cartesian k p | k -> p where
  fork :: forall a c d. k a c -> k a d -> k a (p c d)
  exl :: forall a b. k (p a b) a
  exr :: forall a b. k (p a b) b

class Cartesian k p <= Closed k p r | k -> p r where
  apply :: forall a b. k (p (r a b) a) b
  curry :: forall a b c. k (p a b) c -> k a (r b c)
  uncurry :: forall a b c. k a (r b c) -> k (p a b) c

class Category k <= Terminal k t | k -> t where
  it :: forall a. k a t

class Category k <= Constant k where
  constant :: forall a b. (Unit -> a) -> k b a

class Cartesian k p <= HeytingCategory k p b | k -> p b where
  notC :: k b b
  disjC :: k (p b b) b
  conjC :: k (p b b) b
  impliesC :: k (p b b) b
  ttC :: forall a. k a b
  ffC :: forall a. k a b

class Cartesian k p <= SemigroupCategory k p s | k -> p s where
  appendC :: k (p s s) s

class SemigroupCategory k p m <= MonoidCategory k p m | k -> p m where
  memptyC :: forall a. k a m


-- trivial instances for (->)

instance cartesianFn
  :: Cartesian (->) Tuple where
    fork f g a = Tuple (f a) (g a)
    exl (Tuple a _) = a
    exr (Tuple _ b) = b

instance closedFn
  :: Closed (->) Tuple (->) where
    apply (Tuple f a) = f a
    curry f a b = f (Tuple a b)
    uncurry f (Tuple a b) = f a b

instance terminalFn
  :: Terminal (->) Unit where
    it _ = unit

instance constantFn
  :: Constant (->) where
    constant b _ = b unit

instance heytingCategoryFn
  :: HeytingAlgebra b
  => HeytingCategory (->) Tuple b where
    notC = not
    disjC (Tuple a b) = disj a b
    conjC (Tuple a b) = conj a b
    impliesC (Tuple a b) = implies a b
    ttC _ = tt
    ffC _ = ff

instance semigroupCategoryFn
  :: Semigroup s
  => SemigroupCategory (->) Tuple s where
    appendC (Tuple l r) = append l r

instance monoidCategoryFn
  :: Monoid m
  => MonoidCategory (->) Tuple m where
    memptyC _ = mempty

