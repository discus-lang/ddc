
(* Type Environments.
   Types and kinds are placed in the same envionment. This is to avoid the need
   to shift kind indices in types when pushing new kinds on the stack.

   For example, consider the following expression:

   With a names:
     /\ a1 : k1.
     /\ a2 : k2.
      \ x  : a1 -> C a2.
     exp (/\ a3 : k3. ...)

   When typing "exp" the environment is:
     (a1 : k1), (a2 : k2), (x : a1 -> C a2) |- exp1 (/\ a3 : k3. exp2) :: ...

   When entering into the argument of the application we also push "k3" on the stack.
     (a1 : k1), (a2 : k2), (x : a1 -> C a2), (a3 : k3) |- exp2 :: ...

   In deBruijn representation this is:
     k1, k2, (1 -> C 0), k3 |- exp2

   Note that pushing k3 hasn't affected the indices in the previous type.
   On the other hand, if we had two environments then we'd need to adjust them:
     k1, k2, k3 | (2 -> C 1) |- exp2

 *)

Require Export Ty.

Inductive sort : Type :=
 | SKind
 | SType.


Inductive elem : Type := 
 | EKind : ki -> elem
 | EType : ty -> elem.




 

 


     
   