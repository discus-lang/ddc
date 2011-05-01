
Require Import WellFormed.
Require Import SubstValueValue.
Require Import SubstTypeValue.
Require Import SubstTypeType.
Require Import EsJudge.
Require Import KiJudge.
Require Import TyJudge.
Require Import Exp.
Require Import Base.


Theorem preservation
 :  forall ke te x x' t
 ,  TYPE ke te x t
 -> STEP x x'
 -> TYPE ke te x' t.
Proof.
 intros ke te x x' t HT HS.
 gen ke te x' t.
 induction x; intros.

 Case "XVar".
  inverts HS.

 Case "XLAM".
  inverts HS.

 Case "XAPP".
  inverts HT.
  inverts keep HS.

  SCase "ESAPPLAM".
   inverts H3.
   eapply subst_type_value in H4; eauto.
   rewrite substTE_liftTE in H4. auto.

  SCase "ESAPP1".
   apply TYAPP; eauto.

 Case "XLam".
  inverts HS.

 Case "XApp".
  inverts HT.
  inverts keep HS.
  
  SCase "ESAppLam".
   inverts H3.
   lets D: subst_value_value H9 H5. auto.

  SCase "EsApp1".
   eapply TYApp; eauto.

  SCase "EsApp2".
   eapply TYApp; eauto.
Qed.
  

 
 
