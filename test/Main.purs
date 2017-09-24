module Test.Main where
import Prelude (class Semigroup, pure, unit, Unit)
import Control.Monad.Eff (Eff)
import Type.Program
import Data.Tuple (Tuple)

eg0 :: forall i. i -> i
eg0 = program (Lang :: Lang (Lam V0))

eg1 :: forall a b. a -> b -> b
eg1 = program (Lang :: Lang (Lam (Lam V0)))

eg2 :: forall a b. b -> a -> b
eg2 = program (Lang :: Lang (Lam (Lam V1)))

eg3 :: forall t. Semigroup t => t -> t
eg3 = program (Lang :: Lang (Lam (Append $ (Id △ Id) $ V0)))

eg4 :: forall t. t -> Tuple t t
eg4 = program (Lang :: Lang (Id △ Id))

--eg5 :: forall t. t -> Tuple t t
eg5 = generalise (Lang :: Lang (Lam ((Id △ Id) $ V0)))
--eg5 = program (Lang :: Lang (Apply ○ (Const (Id △ Id) △ Id)))


main :: forall e. Eff e Unit
main = pure unit
