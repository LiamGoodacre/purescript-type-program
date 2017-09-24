module Test.Main where
import Prelude (class Semigroup, pure, unit, Unit)
import Data.CCC as CCC
import Control.Monad.Eff (Eff)
import Type.Program

eg0 :: forall k i.
  CCC.Category k =>
  k i i
eg0 = program (Lang :: Lang (Lam V0))

eg1 :: forall k p r a b.
  CCC.Constant k =>
  CCC.Closed k p r =>
  CCC.Cartesian r p =>
  k a (r b b)
eg1 = program (Lang :: Lang (Lam (Lam V0)))

eg2 :: forall k p r a b.
  CCC.Constant k =>
  CCC.Closed k p r =>
  CCC.Cartesian r p =>
  k b (r a b)
eg2 = program (Lang :: Lang (Lam (Lam V1)))

eg3 :: forall k p r t.
  CCC.Constant k =>
  CCC.Closed k p r =>
  CCC.SemigroupCategory r p t =>
  Semigroup t =>
  k t t
eg3 = program (Lang :: Lang (Lam (Append $ (Id △ Id) $ V0)))

eg4 :: forall k p t.
  CCC.Cartesian k p =>
  k t (p t t)
eg4 = program (Lang :: Lang (Id △ Id))

--eg5 :: forall t. t -> Tuple t t
--eg5 = generalise (Lang :: Lang (Lam ((Id △ Id) $ V0)))
--eg5 = program (Lang :: Lang (Apply ○ (Const (Id △ Id) △ Id)))


main :: forall e. Eff e Unit
main = pure unit
