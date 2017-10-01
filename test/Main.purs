module Test.Main where
import Prelude (class Semigroup, pure, unit, Unit)
import Data.CCC as CCC
import Control.Monad.Eff (Eff)
import Type.Program

eg0 :: forall k i.
  CCC.Category k =>
  k i i
eg0 = program @(Lam V0)

eg1 :: forall k p r a b.
  CCC.Constant k =>
  CCC.Closed k p r =>
  CCC.Cartesian r p =>
  k a (r b b)
eg1 = program @(Lam (Lam V0))

eg2 :: forall k p r a b.
  CCC.Constant k =>
  CCC.Closed k p r =>
  CCC.Cartesian r p =>
  k b (r a b)
eg2 = program @(Lam (Lam V1))

eg3 :: forall k p r t.
  CCC.Constant k =>
  CCC.Closed k p r =>
  CCC.SemigroupCategory r p t =>
  Semigroup t =>
  k t t
eg3 = program @(Lam (Append $ (Id △ Id) $ V0))

eg4 :: forall k p t.
  CCC.Cartesian k p =>
  k t (p t t)
eg4 = program @(Id △ Id)

main :: forall e. Eff e Unit
main = pure unit
