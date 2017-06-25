
Static Semantics
================

The following typing rules are for the explicitly typed form of the core language. We assume that all binders have already been annotated by their types and kinds.


Kinds of Types
--------------

.. code-block:: none

 [ Δ ⊢ t :: k ]


Under kind environment Δ type t has kind k


.. code-block:: none

           TyCon has kind k
  (KiCon) ─────────────────
            Δ ⊢ TyCon :: k

 Con    :: ...
 (→)    :: Data → Data → Data
 Unit   :: Data
 Void   :: Data
 ⋁ k n  :: k1 → k2 ... → kn → k
 ⊥ k    :: k
 ∀ k    :: (k → Data) → k → Data


The kinds of type constructors are given by the above table. The kinds of the primitive type constructors Con are set by the language fragment.

.. code-block:: none

           n:k ∈ Δ
 (KiVar) ────────────
          Δ ⊢ n :: k

The kinds of named type variables are taken from the kind environment.


.. code-block:: none

              Δ, n:k1 ⊢ t :: k2
 (KiAbs) ─────────────────────────────
         Δ ⊢ (λ n : k1. t) :: (k1 → k2)


For a type abstraction, the kind of the parameter is added to the kind environment when checking the body.


.. code-block:: none

         Δ ⊢ t1 :: (k1 → k2)    Δ ⊢ t2 :: k1
 (KiApp) ────────────────────────────────────
                Δ ⊢ (t1 t2) :: k2

For a type-type application, the kind of the parameter of the type function (k1) must match the kind of the argument.


Types of Terms
--------------

.. code-block:: none

 [ Δ | Γ ⊢ e :: t : σ ]

Under kind environment Δ and type environment Γ expression e has type t and effect σ.



.. code-block:: none

                x:t ∈ Γ
 (TyVar)  ────────────────────
           Δ | Γ ⊢ x :: t ! ⊥


The types of named variables are taken from the type environment. Referencing a variable is pure.


.. code-block:: none

            Δ ⊢ t1 :: Data      Δ | Γ, x:t1 ⊢ e :: t2 ! ⊥
 (TyAbs)  ─────────────────────────────────────────────────
               Δ | Γ ⊢ (λ x : t1. e) :: (t1 → t2) ! ⊥

For a term abstraction, the parameter type t1 must have kind Data. The type of the parameter is added to the type environment when checking the body e. The effect of the body must be pure. Forming a term abstraction is pure.


.. code-block:: none

            Δ | Γ ⊢ e1 :: (t1 → t2) ! σ1    Δ | Γ ⊢ e2 :: t1 ! σ2
  (TyAppX) ───────────────────────────────────────────────────────
                  Δ | Γ ⊢ (e1 e2) :: t2 ! (σ1 + σ2)


For a term-term application, the type of the function parameter τ1 must match the type of the argument. The effect of the overall application is the effect of evaluating both the functional expression (σ1) and argument expression (σ2).


.. code-block:: none

           a ∉ Δ     Δ ⊢ k1
           Δ, a:k1 | Γ ⊢ e :: t2 ! ⊥    Δ ⊢ t2 :: Data
 (TyAbsT) ────────────────────────────────────────────────
           Δ | Γ ⊢ (Λ a : k1. e) :: (∀ a : t1. t2)  !  ⊥

For a type abstraction, the parameter kind must be well formed. The kind of the parameter is added to the type environment when checking the body e. The effect of the body must be pure. The type of the body must have kind Data. Forming a type abstraction is pure.


.. code-block:: none

           Δ | Γ ⊢ e1 :: (∀ a : k1. t1) ! σ1    Δ ⊢ t2 :: k1
 (TyAppT) ────────────────────────────────────────────────────
                   Δ | Γ ⊢ e1 t2 :: t1[t2/a] ! σ1

For term-type application, the kind of the type parameter (k1) must match that of the type argument. The type argument is substituted for the formal parameter a in the body type (t1). The effect of the overall application is the effect of evaluating the functional expression (σ1).


.. code-block:: none

          Δ | Γ ⊢ e1 :: t1 ! σ1    Δ | Γ, x1:t1 ⊢ e2 :: t2 ! σ2
 (TyLet) ─────────────────────────────────────────────────────────
               Δ | Γ ⊢ (let x1 = e1 in e2) :: t2 ! σ1 + σ2

The bound variable is in scope in the body of the let-binding. The effect of the overall expression is the effect of evaluating the bound expression and the body.


.. code-block:: none

            { Δ | Γ, { x_i : t_i }^i ⊢ e_i :: t_i ! ⊥ }^i
              Δ | Γ, { x_i : t_i }^i ⊢ e'  :: t'  ! σ'
 (TyLetRec) ──────────────────────────────────────────────────────────
              Δ | Γ ⊢ (letrec { x_i : t_i = e_i }^i in e') :: t' ! σ'

In a letrec every bound variable must be annotated with its type. All the bound variables are in scope in all the bound expressions. All bound expressions must have the types indivated by their corresponding type annotations. All bound expressions in a letrec must be pure. All bound variables are also in scope in the body, and the effect of the overall expression is the effect of evaluating its body.


.. code-block:: none

                     Δ | Γ ⊢ e1 :: t1 ! σ1
 (TyWeakEff) ───────────────────────────────────────
              Δ | Γ ⊢ weakeff σ2 e1 :: t1 ! σ1 + σ2

To weaken the effect of some term e1 we supply a new effect `σ2` which is added to the effect of of the original term `σ1`.

.. code-block:: none

              Δ | Γ ⊢ e :: t ! σ
 (TyBox)  ────────────────────────────
           Δ | Γ ⊢ box e :: S σ t ! ⊥

A boxed term has the type of a suspension, where the effect `σ` and return type `t` of the suspension are the corresponding effect and types of the term being boxed. A boxed term is pure.



.. code-block:: none

           Δ | Γ ⊢ x1 :: S σ1 t1 ! σ2    Γ supports σ1
 (TyRun) ----------------------------------------------
               Δ | Γ ⊢ run e1 :: t1  ! σ1 + σ2


The term to run must have type matching `S σ1 t1`. The type of the result of running it is `t1`. The overall effect of the expression is the effect of computing the suspension `σ2` and the effect of running it `σ1`. The type environment `Γ` must contain capabilities that support the effects σ1.


