
# Disciple Core Language

This is the specification for the desugared, explicitly typed, generic core language. All binders are annotated with their types.

Specific language fragments provide their own set of primitives, and may place further restrictions on how types and term exressions may be formed.

## Abstract Syntax

### Names
```
CON   (c)   ⟶ (named constructors)
VAR   (n)   ⟶ (named variables)
```


### Types
```
TYPE (τ,κ,σ) 
        ::= TYCON                     (type constructors)
         |  VAR                       (type variables)
         |  λ n : TYPE. TYPE          (type abstraction)
         |  TYPE TYPE                 (type application)

TYCON   ::= CON                       (primitive type constructors)
         |  Data | Effect |  Region   (basic kind constructors)
         |  (→)  | Unit   |  Void     (basic type constructors)
         |  Σ TYPE NAT                (least upper bound of types)
         |  ⊥ TYPE                    (least element of a type)
         |  ∀ TYPE                    (universal   quantification constructor)
         |  ∃ TYPE                    (existential quantification constructor)
```

In the grammar for types, the only binding form is that for type abstractions `λn : TYPE.TYPE`. All quantifiers are expressed in terms of generic type abstraction.

The basic kind constructors are `Data`, `Effect` and `Region` for the kind of data, effect and region type respectively. The function constructor `(→)` is usally written infix as per the section on syntactic sure. The `Unit` type classifies a set of values with the single element `()`. The `Void` type classifies the empty set.

The `Σ` type constructor is used to express type sums, where `⊥` expresses an empty sum. Both are annotated with the kind of their result types.

The quantifier constructors `∀` and `∃` are annotated with the kind of their parameters.

#### Syntactic Sugar for Types


```
τ1 → τ2       = (→) τ1 τ2
```
The function type constructor `(→)` is usually written infix.



```
τ1 + τ2       = Σ κ 2 τ1 τ2
τ1 + τ2 + τ3  = Σ κ 3 τ1 τ2 τ3
```

When the kind is clear from context we use the infix `(+)` operator to express type sums.

```
⊥             = ⊥ κ
```

When the kind is clear from context, the kind argument on `(⊥)` is elided.


```
∀ n : κ. τ    = (∀ κ) (λ n : κ. τ)
∃ n : κ. τ    = (∃ κ) (λ n : κ. τ)
```

Quantifiers that bind names are desugared to an application of the associated type constructor and a type abstraction. Representing the different quantifiers as constructors allows us to retain a single binding form for types.



### Terms
```
EXP (x) 
        (data constructors and variables)
        ::= DACON | VAR

        (term abstraction and application)
         |  λ n : TYPE. EXP
         |  EXP EXP

        (type abstraction and application)
         |  Λ n : TYPE. EXP
         |  EXP TYPE

        (let binding)
         |  let    BIND   in EXP
         |  letrec BINDT+ in EXP

        (case matching)
         |  case   EXP   of ALT+

        (private region introduction and region extension)
         |  private  VAR  with  SIG+ in   EXP
         |  extend   VAR  using VAR  with SIG+ in EXP

        (type casts)
         |  weakeff TYPE in EXP
         |  run EXP
         |  box EXP

DACON   ::= CON | ()

BIND    ::= VAR = EXP
BINDT   ::= VAR : TYPE = EXP
SIG     ::= VAR : TYPE

ALT     ::= PAT → EXP
PAT     ::= _ | DACON SIG+ 
```

The syntax of constructors and variables, term abstraction and application, type abstraction and application, case-matching and let-binding is standard.

The `private` construct introduces a new region variable along with capabilities of the given signatures. Both the region variable and names of the capabilities are in scope in the body expression.


## Kinds of Types

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
     Δ, n:κ1 ⊢ τ :: κ2
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


## Types of Terms

```
[ Δ | Γ ⊢ x :: τ : σ ]
```
Under kind environment Δ and type environment Γ expression x has type τ and effect σ.


### TyVar

```
       n:τ ∈ Γ
  ------------------
  Δ | Γ ⊢ n :: τ ! ⊥
```

The types of named variables are taken from the type environment. Referencing a variable is pure.


### TyAbsX
  
```
   Δ ⊢ τ1 :: Data      Δ | Γ, n:τ1 ⊢ x :: τ2 ! ⊥ 
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
       n ∉ Δ     Δ ⊢ κ1    
       Δ, n:κ1 | Γ ⊢ x :: τ2 ! ⊥    Δ ⊢ τ2 :: Data
    ------------------------------------------------ 
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


### TyLet

```
  Δ | Γ ⊢ x1 :: τ1 ! σ1    Δ | Γ, n1: τ1 ⊢ x2 :: τ2 ! σ2
 ---------------------------------------------------------
      Δ | Γ ⊢ (let n1 = x1 in x2) :: τ2 ! σ1 + σ2

```

The bound variable is in scope in the body of the let-binding. The effect of the overall expression is the effect of evaluating the bound expression and the body.


### TyLetRec

```
   { Δ | Γ, { n_i : τ_i }^i ⊢ x_i :: τ_i ! ⊥ }^i
     Δ | Γ, { n_i : τ_i }^i ⊢ x'  :: τ'  ! σ'
 --------------------------------------------------------------
    Δ | Γ ⊢ (letrec { n_i : τ_i = x_i }^i in x') :: τ' ! σ'
```

In a letrec every bound variable must be annotated with its type. All the bound variables are in scope in all the bound expressions. All bound expressions must have the types indivated by their corresponding type annotations. All bound expressions in a letrec must be pure. All bound variables are also in scope in the body, and the effect of the overall expression is the effect of evaluating its body.


### TyWeakEff

```
 Δ | Γ ⊢ x1 :: τ1 ! σ1
 --------------------------------------
 Δ | Γ ⊢ weakeff σ2 x1 :: τ1 ! σ1 + σ2
```

To weaken the effect of some term `x1` we supply a new effect `σ2` which is added to the effect of of the original term `σ1`.

### TyBox

```
  Δ | Γ ⊢ x :: τ ! σ 
 ----------------------------
  Δ | Γ ⊢ box x :: S σ τ ! ⊥
```

A boxed term has the type of a suspension, where the effect `σ` and return type `τ` of the suspension are the corresponding effect and types of the term being boxed. A boxed term is pure.


### TyRun

```
  Δ | Γ ⊢ x1 :: S σ1 τ1 ! σ2    Γ supports σ1
 ----------------------------------------------
  Δ | Γ ⊢ run x1 :: τ1  ! σ1 + σ2
```

The term to run must have type matching `S σ1 τ1`. The type of the result of running it is `τ1`. The overall effect of the expression is the effect of computing the suspension `σ2` and the effect of running it `σ1`. The type environment `Γ` must contain capabilities that support the effects σ1.




