module Type.Program.Generalise where
import Type.Program.Lang
import Type.Utils (class EqNat, class If, Nat, S, Z, kind Nat)

foreign import kind Pairing
data Pairing (pairing :: Pairing) = Pairing
foreign import data None :: Pairing
foreign import data First :: Pairing
foreign import data Second :: Pairing

class GetPairing (v :: Nat)
                 (n :: Nat)
                 (p :: Pairing)
                 | v n -> p

instance getPairingInstance
  :: ( EqNat v n vn
     , EqNat v (S n) vSn
     , If vn (Pairing Second) e (Pairing o)
     , If vSn (Pairing First) (Pairing None) e
     )
  => GetPairing v n o

getPairing :: forall v n p.
  GetPairing v n p =>
  Nat v ->
  Nat n ->
  Pairing p
getPairing _ _ = Pairing


class SubstitutePairing (p :: Pairing)
                        (v :: Nat)
                        (n :: Nat)
                        (o :: Lang)
                        | p v n -> o

substitutePairing :: forall p v n o.
  SubstitutePairing p v n o =>
  Pairing p ->
  Nat v ->
  Nat n ->
  Lang o
substitutePairing _ _ _ = Lang

instance substitutePairingNone
  :: SubstitutePairing None v n (Var v)

instance substitutePairingFirst
  :: SubstitutePairing First v n (App Exl (Var n))

instance substitutePairingSecond
  :: SubstitutePairing Second v n (App Exr (Var n))


class SubstitutePair (lc :: Lang)
                     (n :: Nat)
                     (out :: Lang)
                     | lc n -> out

substitutePair :: forall lc n out.
  SubstitutePair lc n out =>
  Lang lc ->
  Nat n ->
  Lang out
substitutePair _ _ = Lang

instance substitutePairVar
  :: ( GetPairing v n p
     , SubstitutePairing p v n o )
  => SubstitutePair (Var v) n o
else
instance substitutePairLam
  :: SubstitutePair b (S n) o
  => SubstitutePair (Lam b) n (Lam o)
else
instance substitutePairApp
  :: ( SubstitutePair l n l'
     , SubstitutePair r n r'
     )
  => SubstitutePair (App l r) n (App l' r')
else
instance substitutePairElse
  :: SubstitutePair t n t


-- generalise to ccc

class Generalise (lc :: Lang)
                 (ccc :: Lang)
                 | lc -> ccc

class GeneraliseLam (lc :: Lang)
                    (ccc :: Lang)
                    | lc -> ccc

generalise :: forall lc ccc.
  Generalise lc ccc =>
  Lang lc ->
  Lang ccc
generalise _ = Lang

instance generaliseLamLam
  :: (SubstitutePair b Z b', GeneraliseLam b' c)
  => GeneraliseLam (Lam b) (Curry c)
else
instance generaliseLamVarZ
  :: GeneraliseLam (Var Z) Id
else
instance generaliseLamApp
  :: (GeneraliseLam l l', GeneraliseLam r r')
  => GeneraliseLam (App l r) (Apply ○ (l' △ r'))
else
instance generaliseLamOther
  :: Generalise i c
  -- todo - need to find all free vars in c
  --        if current var isn't mentioned then use Const
  --        otherwise Lam
  --        also need to rename other free vars
  --        because we may be removing a binder
  --        alternatively we could make Const
  --        introduce a binder that can't be used
  => GeneraliseLam i (Const c)

instance generaliseLam     :: GeneraliseLam b b' => Generalise (Lam b) b'
instance generaliseVar     :: Generalise (Var n) (Var n)
instance generaliseApp     :: (Generalise l l', Generalise r r') => Generalise (App l r) (App l' r')
instance generaliseCompose :: (Generalise l l', Generalise r r') => Generalise (l ○ r) (l' ○ r')
instance generaliseId      :: Generalise Id Id
instance generaliseFork    :: (Generalise l l', Generalise r r') => Generalise (l △ r) (l' △ r')
instance generaliseExl     :: Generalise Exl Exl
instance generaliseExr     :: Generalise Exr Exr
instance generaliseApply   :: Generalise Apply Apply
instance generaliseCurry   :: Generalise b b' => Generalise (Curry b) (Curry b')
instance generaliseUncurry :: Generalise b b' => Generalise (Uncurry b) (Uncurry b')
instance generaliseIt      :: Generalise It It
instance generaliseConst   :: Generalise b b' => Generalise (Const b) (Const b')
instance generaliseNot     :: Generalise Not Not
instance generaliseDisj    :: Generalise Disj Disj
instance generaliseConj    :: Generalise Conj Conj
instance generaliseImplies :: Generalise Implies Implies
instance generaliseTrue    :: Generalise True True
instance generaliseFalse   :: Generalise False False
instance generaliseAppend  :: Generalise Append Append
instance generaliseMempty  :: Generalise Mempty Mempty
