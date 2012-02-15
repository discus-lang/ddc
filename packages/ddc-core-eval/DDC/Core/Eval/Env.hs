
-- | Primitive types and operators for the core language evaluator.
--
--   These are only a subset of the primitives supported by the real compiler, there's just
--   enough to experiment with the core language. 
--
module DDC.Core.Eval.Env
        ( -- * Data Type Definitions.
          primDataDefs

          -- * Kind environment.
        , primKindEnv
        , kindOfPrimName

          -- * Type Environment.
        , primTypeEnv
        , typeOfPrimName
        , arityOfName)
where
import DDC.Core.Eval.Compounds
import DDC.Core.Eval.Name
import DDC.Core.DataDef
import DDC.Type.Exp
import DDC.Type.Compounds
import DDC.Type.Env             (Env)
import qualified DDC.Type.Env   as Env


-- DataDefs -------------------------------------------------------------------
-- | Data type definitions for:
--
-- @  Type   Constructors
--  ----   ------------
--  Unit   ()
--  Int    0 1 2 3 ...
--  List   Nil Cons
-- @
primDataDefs :: DataDefs Name
primDataDefs
 = fromListDataDefs
        -- Unit
        [ DataDef
                (NamePrimCon PrimTyConUnit)
                []
                (Just [ (NamePrimCon PrimDaConUnit, []) ])
        
        -- Int
        , DataDef
                (NamePrimCon PrimTyConInt)
                [kRegion]
                Nothing

        -- List
        , DataDef
                (NamePrimCon PrimTyConList)
                [kRegion, kData]
                (Just   [ (NamePrimCon PrimDaConNil,  []) 
                        , (NamePrimCon PrimDaConCons, [tList (tIx kRegion 1) (tIx kData 0)])])
        ]


-- Kinds ----------------------------------------------------------------------
-- | Kind environment containing kinds of primitive data types.
primKindEnv :: Env Name
primKindEnv = Env.setPrimFun kindOfPrimName Env.empty


-- | Take the kind of a primitive name.
--
--   Returns `Nothing` if the name isn't primitive. 
--
kindOfPrimName :: Name -> Maybe (Kind Name)
kindOfPrimName nn
 = case nn of
        NameRgn _
         -> Just $ kRegion

        -- Unit
        NamePrimCon PrimTyConUnit
         -> Just $ kData
        
        -- List
        NamePrimCon PrimTyConList
         -> Just $ kRegion `kFun` kData `kFun` kData

         -- Int
        NamePrimCon PrimTyConInt
         -> Just $ kFun kRegion kData

        _ -> Nothing




-- Types ----------------------------------------------------------------------
-- | Type environment containing types of primitive data constructors as well
--   as the following primitive operators:
--
--  @negInt, addInt, subInt, mulInt, divInt, eqInt, updateInt@
--
--   It also contains types for the primitive capability constructors:
--
--  @Global\#, Const\#, Mutable\#, Lazy\#, Manifest\#@
-- 
primTypeEnv :: Env Name
primTypeEnv = Env.setPrimFun typeOfPrimName Env.empty


-- | Take the type of a primitive name.
--
--   Returns `Nothing` if the name isn't primitive. 
--
typeOfPrimName :: Name -> Maybe (Type Name)
typeOfPrimName nn
 = case nn of
        -- Unit
        NamePrimCon PrimDaConUnit
         -> Just $ tUnit 

        
        -- List
        NamePrimCon PrimDaConNil        
         -> Just $ tForalls [kRegion, kData] $ \[tR, tA]
                -> tFun tUnit (tAlloc tR)
                              (tBot kClosure)
                 $ tList tR tA

        NamePrimCon PrimDaConCons
         -> Just $ tForalls [kRegion, kData] $ \[tR, tA] 
                -> tFun tA            (tBot kEffect)
                                      (tBot kClosure)
                 $ tFun (tList tR tA) (tSum kEffect  [tAlloc   tR])
                                      (tSum kClosure [tDeepUse tA])
                 $ tList tR tA

        -- Int
        NameInt _
         -> Just $ tForall kRegion
          $ \r  -> tFun tUnit (tAlloc r)
                              (tBot kClosure)
                 $ tInt r

        -- negInt
        NamePrimOp PrimOpNegInt
         -> Just $ tForalls [kRegion, kRegion] $ \[r1, r0]
                -> tFun (tInt r1) (tSum kEffect  [tRead r1, tAlloc r0])
                                  (tBot kClosure)
                      $ (tInt r0)

        -- add, sub, mul, div, eq
        NamePrimOp p
         | elem p [PrimOpAddInt, PrimOpSubInt, PrimOpMulInt, PrimOpDivInt, PrimOpEqInt]
         -> Just $ tForalls [kRegion, kRegion, kRegion] $ \[r2, r1, r0] 
                -> tFun (tInt r2) (tBot kEffect)
                                  (tBot kClosure)
                 $ tFun (tInt r1) (tSum kEffect  [tRead r2, tRead r1, tAlloc r0])
                                  (tSum kClosure [tUse r2])
                 $ tInt r0

        -- update :: [r1 r2 : %]. Mutable r1 => Int r1 -> Int r2 -(Write r1 + Read r2 | Share r1)> ()
        NamePrimOp PrimOpUpdateInt
         -> Just $ tForalls [kRegion, kRegion] $ \[r1, r2]
                -> tImpl (tMutable r1)
                $  tFun  (tInt r1) (tBot kEffect)
                                   (tBot kClosure)
                $  tFun  (tInt r2) (tSum kEffect  [tWrite r1, tRead r2])
                                   (tSum kClosure [tUse r1])
                $  tUnit

        NameCap CapGlobal       -> Just $ tForall kRegion $ \r -> tGlobal   r
        NameCap CapConst        -> Just $ tForall kRegion $ \r -> tConst    r
        NameCap CapMutable      -> Just $ tForall kRegion $ \r -> tMutable  r
        NameCap CapLazy         -> Just $ tForall kRegion $ \r -> tLazy     r
        NameCap CapManifest     -> Just $ tForall kRegion $ \r -> tManifest r

        _ -> Nothing



-- | Take the arity of a primitive name.
---
-- TODO: determine this from the type.
arityOfName :: Name -> Maybe Int
arityOfName n
 = case n of
        NameLoc{}                       -> Just 0
        NameRgn{}                       -> Just 0
        NameInt{}                       -> Just 2

        NamePrimCon PrimDaConUnit       -> Just 0        
        NamePrimCon PrimDaConNil        -> Just 3

        NamePrimCon PrimDaConCons       -> Just 4

        NamePrimOp p
         | elem p [ PrimOpAddInt, PrimOpSubInt, PrimOpMulInt, PrimOpDivInt
                  , PrimOpEqInt]
         -> Just 5
         
        NamePrimOp PrimOpUpdateInt
         -> Just 5        
         
        _ -> Nothing

