
Require Import Coq.Lists.List.


Require Import ZArith.


Inductive thing : Type := 
 | ThingNat   : nat   -> thing
 | ThingApp   : thing -> thing -> thing
 | ThingList  : list thing -> thing.



Fixpoint got_fives (tt: thing) : Prop :=
 match tt with 
 | ThingNat  i       => i = 5
 | ThingApp  t1 t2   => got_fives t1 /\ got_fives t2

 | ThingList ts     
 => (fix more (ts': list thing) : Prop :=
     match ts' with 
     | nil           => True
     | t :: ts''     => got_fives t /\ more ts''
     end) ts
 end.
