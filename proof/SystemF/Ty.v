
Require Export Name.

(* Kinds **************************************************)
Inductive ki : Type :=
 | KStar   : ki.


(* Types **************************************************)
Inductive ty : Type :=
 | TCon    : name -> ty
 | TVar    : name -> ty
 | TForall : name -> ty -> ty
 | TFun    : ty   -> ty -> ty.


(* Free variables *****************************************)
Inductive freeT : name -> ty -> Prop :=
 | FreeT_var
   :  forall x, freeT x (TVar x)

 | FreeT_forall
   :  forall x1 x2 t1
   ,  x1 <> x2
   -> freeT x1 t1
   -> freeT x1 (TForall x2 t1)

 | FreeT_fun1
   :  forall x T1 T2
   ,  freeT x T1 -> freeT x (TFun T1 T2)

 | FreeT_fun2 
   :  forall x T1 T2
   ,  freeT x T2 -> freeT x (TFun T1 T2).

Hint Constructors freeT.
Hint Resolve FreeT_var.


(* Binding variables ***************************************)
Inductive bindsT : name -> ty -> Prop :=
 | BindsT_forall_bound
   :  forall a t1
   ,  bindsT a (TForall a t1)

 | BindsT_forall
   :  forall a b t1
   ,  bindsT a t1
   -> bindsT a (TForall b t1)

 | BindsT_fun
   :  forall a T1 T2
   ,  bindsT a T1 \/ bindsT a T2
   -> bindsT a (TFun T1 T2).

Hint Constructors bindsT.


(* Substitution of Types in Types *************************)
Fixpoint substTT (x : name) (S : ty) (T : ty) : ty :=
  match T with
    |  TCon x
    => TCon x

    |  TVar x'      
    => if beq_name x x' then S else T

    |  TForall x' T1
    => if beq_name x x'
        then TForall x' T1
        else TForall x' (substTT x S T1)  

    |  TFun T1 T2 
    => TFun (substTT x S T1) (substTT x S T2)
  end.


(* examples ***********************************************)
Notation tA := (TVar (Name 0)).
Notation tB := (TVar (Name 1)).
Notation tC := (TVar (Name 2)).
