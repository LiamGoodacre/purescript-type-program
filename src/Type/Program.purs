module Type.Program
  ( module Type.Program.Lang
  , module Type.Program.Reflect
  , module Type.Program.Generalise
  , program
  ) where

import Type.Program.Lang
import Type.Program.Reflect (class Reflect, reflect)
import Type.Program.Generalise (class Generalise, generalise)

program :: forall prog gen out.
  Generalise prog gen =>
  Reflect gen out =>
  Lang prog -> out
program _ = reflect (Lang :: Lang gen)
