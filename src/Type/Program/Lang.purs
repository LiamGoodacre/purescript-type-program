module Type.Program.Lang where
import Type.Utils

-- type level language

foreign import kind Lang

-- lambda core
foreign import data Lam :: Lang -> Lang
foreign import data Var :: Nat -> Lang
foreign import data App :: Lang -> Lang -> Lang

-- semigroupoid
foreign import data Compose :: Lang -> Lang -> Lang

-- category
foreign import data Id :: Lang

-- cartesian
foreign import data Fork :: Lang -> Lang -> Lang
foreign import data Exl :: Lang
foreign import data Exr :: Lang

-- closed
foreign import data Apply :: Lang
foreign import data Curry :: Lang -> Lang
foreign import data Uncurry :: Lang -> Lang

-- terminal
foreign import data It :: Lang

-- constant
foreign import data Const :: Lang -> Lang

-- boolean
foreign import data Not :: Lang
foreign import data Disj :: Lang
foreign import data Conj :: Lang
foreign import data Implies :: Lang
foreign import data True :: Lang
foreign import data False :: Lang

-- semigroup
foreign import data Append :: Lang

-- monoid
foreign import data Mempty :: Lang

infixr 4 type App as $
infixl 4 type Compose as ○
infix 4 type Fork as △

type V0 = Var Z
type V1 = Var (S Z)
type V2 = Var (S (S Z))
