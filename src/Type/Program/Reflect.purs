module Type.Program.Reflect where
import Type.Program.Lang
import Data.CCC as C
import Type.Utils (Nat(..), S, Z, kind Nat)

data TNil = TNil
data TCons h t = TCons h t

class Index (env :: Type)
            (nat :: Nat)
            (out :: Type)
            | nat env -> out
  where
    index :: env -> Nat nat -> out

instance indexZ ::
  Index (TCons h t) Z h
  where
    index (TCons h _) _ = h

instance indexS ::
  Index t n out =>
  Index (TCons h t) (S n) out
  where
    index (TCons _ t) _ = index t (Nat :: Nat n)


class
  ReflectLang (env :: Type)
              (lang :: Lang)
              (typ :: Type)
              | lang -> typ
  where
    reflectLang :: env -> Lang lang -> typ


instance reflectLangLam
  :: ReflectLang (TCons dom env) body cod
  => ReflectLang env (Lam body) (dom -> cod)
  where
    reflectLang env _ =
      \dom -> reflectLang (TCons dom env)
                          (Lang :: Lang body)

instance reflectLangVar
  :: Index env nat typ
  => ReflectLang env (Var nat) typ
  where
    reflectLang env _ =
      index env (Nat :: Nat nat)

instance reflectLangApp
  :: ( ReflectLang env fn (dom -> cod)
     , ReflectLang env arg dom )
  => ReflectLang env (App fn arg) cod
  where
    reflectLang env _ =
      reflectLang env (Lang :: Lang fn)
                      (reflectLang env (Lang :: Lang arg))

instance reflectLangCompose
  :: ( C.Semigroupoid k
     , ReflectLang env l (k y z)
     , ReflectLang env r (k x y)
     )
  => ReflectLang env (Compose l r) (k x z) where
    reflectLang env _ = C.compose (reflectLang env (Lang :: Lang l))
                                  (reflectLang env (Lang :: Lang r))

instance reflectLangId
  :: C.Category k
  => ReflectLang env Id (k i i) where
  reflectLang _ _ = C.id

instance reflectLangFork
  :: ( C.Cartesian k p
     , ReflectLang env f (k a c)
     , ReflectLang env g (k a d)
     )
  => ReflectLang env (f △ g) (k a (p c d)) where
    reflectLang env _ =
      C.fork (reflectLang env (Lang :: Lang f))
             (reflectLang env (Lang :: Lang g))

instance reflectLangIt
  :: C.Terminal k t
  => ReflectLang env It (k a t) where
    reflectLang _ _ = C.it

instance reflectLangConst
  :: ( C.Constant k
     , ReflectLang env s a
     )
  => ReflectLang env (Const s) (k b a) where
    reflectLang env _ = C.constant \_ -> reflectLang env (Lang :: Lang s)

instance reflectLangExl
  :: C.Cartesian k p
  => ReflectLang env Exl (k (p a b) a) where
    reflectLang _ _ = C.exl

instance reflectLangExr
  :: C.Cartesian k p
  => ReflectLang env Exr (k (p a b) b) where
    reflectLang _ _ = C.exr

instance reflectLangApply
  :: C.Closed k p r
  => ReflectLang env Apply (k (p (r a b) a) b) where
    reflectLang _ _ = C.apply

instance reflectLangCurry
  :: ( C.Closed k p r
     , ReflectLang env e (k (p a b) c)
     )
  => ReflectLang env (Curry e) (k a (r b c)) where
    reflectLang env _ = C.curry (reflectLang env (Lang :: Lang e))

instance reflectLangUncurry
  :: ( C.Closed k p r
     , ReflectLang env e (k a (r b c))
     )
  => ReflectLang env (Uncurry e) (k (p a b) c) where
    reflectLang env _ = C.uncurry (reflectLang env (Lang :: Lang e))

instance reflectLangNot
  :: C.HeytingCategory k p b
  => ReflectLang env Not (k b b) where
    reflectLang _ _ = C.notC

instance reflectLangDisj
  :: C.HeytingCategory k p b
  => ReflectLang env Disj (k (p b b) b) where
    reflectLang _ _ = C.disjC

instance reflectLangConj
  :: C.HeytingCategory k p b
  => ReflectLang env Conj (k (p b b) b) where
    reflectLang _ _ = C.conjC

instance reflectLangImplies
  :: C.HeytingCategory k p b
  => ReflectLang env Implies (k (p b b) b) where
    reflectLang _ _ = C.impliesC

instance reflectLangTrue
  :: C.HeytingCategory k p b
  => ReflectLang env True (k a b) where
    reflectLang _ _ = C.ttC

instance reflectLangFalse
  :: C.HeytingCategory k p b
  => ReflectLang env False (k a b) where
    reflectLang _ _ = C.ffC

instance reflectLangAppend
  :: C.SemigroupCategory k p s
  => ReflectLang env Append (k (p s s) s) where
    reflectLang _ _ = C.appendC

instance reflectLangMempty
  :: C.MonoidCategory k p m
  => ReflectLang env Mempty (k a m) where
    reflectLang _ _ = C.memptyC


class
  ReflectLang TNil lang typ <=
  Reflect (lang :: Lang)
          (typ :: Type)
          | lang -> typ
  where
    reflect :: Lang lang -> typ

instance reflectInstance
  :: ReflectLang TNil lang typ
  => Reflect lang typ
  where
    reflect lang = reflectLang TNil lang


-- examples

eg0 :: forall k p b. C.HeytingCategory k p b => k b b
eg0 = reflect (Lang :: Lang (Compose Conj (Id △ Not)))

eg0' :: Boolean
eg0' = eg0 true

eg1 :: forall k a. C.Category k => k a a
eg1 = reflect (Lang :: Lang (Compose Id Id))

eg2 :: forall k p a. C.Cartesian k p => k a (p a a)
eg2 = reflect (Lang :: Lang (Id △ Id))

eg3 :: forall k p r a b. C.Closed k p r => k (p a (r a b)) b
eg3 = reflect (Lang :: Lang (Apply ○ (Exr △ Exl)))

eg4 :: forall k p r a b. C.Closed k p r => k (p (r a b) a) b
eg4 = reflect (Lang :: Lang (Apply ○ Id))

eg5 :: forall a b. b -> a -> b
eg5 = reflect (Lang :: Lang (Lam (Lam (Var (S Z)))))

eg6 :: forall a b. a -> b -> b
eg6 = reflect (Lang :: Lang (Lam (Lam (Var Z))))

