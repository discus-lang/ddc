
# Disciple Core Language

This is the specification for the desugared, explicitly typed, generic core language. All binders are annotated with their types.

Specific language fragments provide their own set of primitives, and may place further restrictions on how types and term exressions may be formed.

## Grammar
```
VAR (n)  ⟶ (named variables)
CON (c)  ⟶ (named constructors)

TYPE (τ,κ,σ) 
        ::= TYCON                               -- Type Constructors
         |  VAR                                 -- Type Variables
         |  λ n : TYPE. EXP                     -- Type Abstraction
         |  TYPE TYPE                           -- Type Application

TYCON   ::= CON                                 -- Primitive type constructor.
         |  Data                                -- Data kind
         |  Effect                              -- Effect kind
         |  Region                              -- Region kind
         |  (→)                                 -- Function type constructor
         |  Unit                                -- Unit type
         |  Void                                -- Void type
         |  Σ TYPE NAT                          -- Least-upper-bound of types
         |  ⊥ TYPE                              -- Least type
         |  ∀ TYPE                              -- Forall quantification
         |  ∃ TYPE                              -- Exists quantification

EXP (x) ::= CON                                 -- Data Constructors
         |  VAR                                 -- Term Variables
         |  λ n : TYPE. EXP                     -- Type  Abstraction
         |  EXP EXP                             -- Term-Term Application
         |  Λ n : TYPE. EXP                     -- Value Abstraction
         |  EXP TYPE                            -- Term-Type Application 
         |  let  BIND  in  EXP                  -- Let binding
         |  letrec  BINDT+  in  EXP             -- Recursive let-binding
```

## Kinding

```
[ Δ ⊢ τ :: κ ]
```

Under kind environment Δ type τ has kind κ.

### KiCon


```
  TyCon has kind κ
 -------------------
   Δ ⊢ TyCon :: κ

 CON    :: ...
 (→)    :: Data → Data → Data
 Unit   :: Data
 Void   :: Data
 Σ κ n  :: κ1 → κ2 ... → κn → κ
 ⊥ κ    :: κ
 ∀ κ    :: (κ → Data) → κ → Data
 ∃ κ    :: (κ → Data) → κ → Data
```

The kinds of type constructors are given by the above table. The kinds of the primitive type constructors CON are defined by the language fragment.


### KiVar

```
  n : κ ∈ Δ
  -----------
  Δ ⊢ n :: κ
```

The kinds of named type variables are taken from the kind environment.


### KiAbs

```
     Δ, n : κ1 ⊢ τ :: κ2
  ---------------------------
  Δ ⊢ λ n : κ1. τ :: κ1 → κ2
```

For a type abstraction, the kind of the parameter is added to the kind environment when checking the body.

### KiApp

```
  Δ ⊢ τ1 :: κ1 → κ2   Δ ⊢ τ2 :: κ1
  ---------------------------------
       Δ ⊢ τ1 τ2 :: κ2
```

For a type-type application, the kind of the parameter of the type function (κ1) must match the kind of the argument.


## Typing

```
[ Δ | Γ ⊢ x :: τ : σ ]
```
Under kind environment Δ and type environment Γ expression x has type τ and effect σ.


### TyVar

```
      n : τ ∈ Γ
  ------------------
  Δ | Γ ⊢ n :: τ ! ⊥
```

The types of named variables are taken from the type environment. Referencing a variable is pure.


### TyAbsX
  
```
   Δ ⊢ τ1 :: Data      Δ | Γ, n : τ1 ⊢ x :: τ2 ! ⊥ 
  --------------------------------------------------
     Δ | Γ ⊢ (λ n : τ1. x) :: (τ1 → τ2) ! ⊥
```

For a term abstraction, the parameter type τ1 must have kind Data. The type of the parameter is added to the type environment when checking the body x. The effect of the body must be pure. Forming a term abstraction is pure.


### TyAppX

```
  Δ | Γ ⊢ x1 :: (τ1 → τ2) ! σ1    Δ | Γ ⊢ x2 :: τ1 ! σ2
  -----------------------------------------------------
             Δ | Γ ⊢ (x1 x2) :: τ2 ! (σ1 + σ2)
```

For a term-term application, the type of the function parameter τ1 must match the type of the argument. The effect of the overall application is the effect of evaluating both the functional expression (σ1) and argument expression (σ2).


### TyAbsT

```
    Δ ⊢ κ1    n : κ1 ⊢ x :: τ2 ! ⊥    Δ ⊢ τ2 :: Data
  -----------------------------------------------------
       Δ | Γ ⊢ (Λ n : κ1. x) :: (∀ n : τ1. τ2)  !  ⊥
```

For a type abstraction, the parameter kind must be well formed. The kind of the parameter is added to the type environment when checking the body x. The effect of the body must be pure. The type of the body must have kind Data. Forming a type abstraction is pure.


### TyAppT

```
   Δ | Γ ⊢ x1 :: (∀ n : κ1. τ1) ! σ1    Δ ⊢ τ2 :: κ1
  -----------------------------------------------------
           Δ | Γ ⊢ x1 τ2 :: τ1[τ2/n] ! σ1
```

For term-type application, the kind of the type parameter (κ1) must match that of the type argument. The type argument is substituted for the formal parameter n in the body type (τ1). The effect of the overall application is the effect of evaluating the functional expression (σ1).




