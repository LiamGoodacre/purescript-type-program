module Type.Utils where

foreign import kind Bool
data Bool (bool :: Bool) = Bool
foreign import data T :: Bool
foreign import data F :: Bool

class If (b :: Bool) (t :: Type) (e :: Type) (o :: Type) | b t e -> o
instance ifT :: If T t e t
instance ifF :: If F t e e

foreign import kind Nat
data Nat (nat :: Nat) = Nat
foreign import data Z :: Nat
foreign import data S :: Nat -> Nat

class EqNat (l :: Nat) (r :: Nat) (b :: Bool) | l r -> b
instance eqNatZZ :: EqNat Z Z T
instance eqNatSZ :: EqNat (S k) Z F
instance eqNatZS :: EqNat Z (S k) F
instance eqNatSS :: EqNat l r b => EqNat (S l) (S r) b
