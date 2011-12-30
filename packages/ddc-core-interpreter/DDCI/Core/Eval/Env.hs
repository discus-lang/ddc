
-- | Primitive types and operators for the interpreter.
--
--   These are only a subset of the primitives supported by the real compiler, there's just
--   enough to experiment with the core language. When we end up wanting to interpret full
--   Disciple programs, we should use the primops defined by the real compiler.
--
module DDCI.Core.Eval.Env
        ( primEnv
        , typeOfPrimName
        , arityOfName)
where
import DDCI.Core.Eval.Compounds
import DDCI.Core.Eval.Name
import DDC.Type.Exp
import DDC.Type.Compounds
import DDC.Type.Env             (Env)
import qualified DDC.Type.Env   as Env


-- | Environment containing just the primitive names.
primEnv :: Env Name
primEnv = Env.setPrimFun typeOfPrimName Env.empty


-- | Take the type of a primitive name.
--
--   Returns `Nothing` if the name isn't primitive. During checking, non-primitive
--   names should be bound in the type environment.
--
typeOfPrimName :: Name -> Maybe (Type Name)
typeOfPrimName nn
 = case nn of
        NameRgn _
         -> Just $ kRegion

        -- Unit -----------------------------
        NamePrimCon PrimTyConUnit
         -> Just $ kData

        NamePrimCon PrimDaConUnit
         -> Just $ tUnit 

        
        -- List -----------------------------
        NamePrimCon PrimTyConList
         -> Just $ kRegion `kFun` kData `kFun` kData

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

        -- Int ------------------------------
        NamePrimCon PrimTyConInt
         -> Just $ kFun kRegion kData

        NameInt _
         -> Just $ tForall kRegion
          $ \r  -> tFun tUnit (tAlloc r)
                              (tBot kClosure)
                 $ tInt r

        -- neg
        NamePrimOp PrimOpNegInt
         -> Just $ tForalls [kRegion, kRegion] $ \[r1, r0]
                -> tFun (tInt r1) (tSum kEffect  [tRead r1, tAlloc r0])
                                  (tBot kClosure)
                      $ (tInt r0)

        -- add, sub
        NamePrimOp p
         | elem p [PrimOpAddInt, PrimOpSubInt, PrimOpMulInt, PrimOpDivInt]
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
                 
        _ -> Nothing


-- TODO: determine this from the type.
arityOfName :: Name -> Maybe Int
arityOfName n
 = case n of
        NameLoc{}                       -> Just 0
        NameRgn{}                       -> Just 0
        NameInt{}                       -> Just 2

        NamePrimCon PrimDaConUnit       -> Just 0        
        NamePrimCon PrimDaConNil        -> Just 2

        NamePrimCon PrimDaConCons       -> Just 4

        NamePrimOp p
         | elem p [PrimOpAddInt, PrimOpSubInt, PrimOpMulInt, PrimOpDivInt]
         -> Just 5
         
        NamePrimOp PrimOpUpdateInt
         -> Just 5        
         
        _ -> Nothing



