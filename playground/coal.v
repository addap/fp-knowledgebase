Set Implicit Arguments.

Section Lens.

Variable B : Type.

Definition W a := ((B -> a) * B)%type.

Definition extract (a:Type) (x : W a) : a := fst x (snd x).

Definition fmap (a b:Type) (f : a -> b) (x : W a) : W b := pair (fun z => f (fst x z)) (snd x).

Definition duplicate (a:Type) (x : W a) : W (W a) := pair (pair (fst x)) (snd x) .

Definition extend (a b:Type) (f : W a -> b) (x : W a) : W b := fmap f (duplicate x).

Variable A : Type.

Variable l : A -> W A.

Definition rd (r : A -> W A) (a:A) : B := snd (r a).
Definition wr (r : A -> W A) (a:A) (b:B) : A := fst (r a) b.

Section Iso1.

Hypothesis coalgebra1 : forall a, extract (l a) = a.
Hypothesis coalgebra2 : forall a, fmap l (l a) = duplicate (l a).

Lemma lawA : forall s t, rd l (wr l s t) = t.
Proof.
intros s t.
generalize (coalgebra2 s).
unfold rd, wr.
destruct (l s) as [f b].
unfold fmap, duplicate.
simpl.
inversion 1.
change (l (f t)) with ((fun z => l (f z)) t).
rewrite H1.
reflexivity.
Qed.

Lemma lawB : forall s, wr l s (rd l s) = s.
Proof.
firstorder.
Qed.

Lemma lawC : forall s t1 t2, wr l (wr l s t1) t2 = wr l s t2.
Proof.
intros s t.
generalize (coalgebra2 s).
unfold rd, wr.
destruct (l s) as [f b].
unfold fmap, duplicate.
simpl.
inversion 1.
change (l (f t)) with ((fun z => l (f z)) t).
rewrite H1.
reflexivity.
Qed.

End Iso1.

Section Iso2.

Hypothesis law1 : forall s t, rd l (wr l s t) = t.
Hypothesis law2 : forall s, wr l s (rd l s) = s.
Hypothesis law3 : forall s t1 t2, wr l (wr l s t1) t2 = wr l s t2.

Lemma coalgebraA : forall a, extract (l a) = a.
Proof.
firstorder.
Qed.

(* One day we will have observational type theory, but for now ... *)
Require Import FunctionalExtensionality.

Lemma coalgebraB : forall a, fmap l (l a) = duplicate (l a).
Proof.
intros a.
apply injective_projections; auto.
apply functional_extensionality. intros x.
simpl.
apply injective_projections.
 apply functional_extensionality; intros y.
 firstorder.
firstorder.
Qed.

End Iso2.
End Lens.
