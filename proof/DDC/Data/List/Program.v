
Require Import DDC.Data.List.Base.
Require Import DDC.Data.List.Map.


Lemma match_option
 : forall {A} {B} (f: A -> B) (xs: list A) ix y
 , f (match get ix xs         with | Some x => x | None  => y   end)
 =   (match get ix (map f xs) with | Some x => x | None  => f y end).
Proof.
 intros.
 remember (get ix xs) as Get.
 destruct Get.
  symmetry in HeqGet.
  apply (get_map f) in HeqGet.
  rewrite HeqGet. auto.

  remember (get ix (map f xs)) as X.
  destruct X.
   symmetry in HeqX.   eapply get_length_more in HeqX.
   symmetry in HeqGet. eapply get_none_length in HeqGet.
   rewrite map_length in HeqX. exfalso. omega.
   auto.
Qed.
