
-- Suppress Data.Monoid warnings during GHC 8.4.1 transition
{-# OPTIONS  -Wno-unused-imports #-}

module DDC.Core.Collect.Support
        ( Support       (..)
        , SupportX      (..)
        , supportEnvFlags
        , emptySupport
        , unionSupport
        , supportOfImportCap
        , supportOfImportValue)
where
import DDC.Core.Module
import DDC.Core.Exp.Annot
import DDC.Core.Collect.FreeT
import Data.Set                 (Set)
import DDC.Type.Env             (KindEnv, TypeEnv)
import qualified DDC.Type.Env   as Env
import qualified Data.Set       as Set
import Data.Maybe

-- GHC 8.2 -> 8.4 transition.
import Data.Semigroup                   (Semigroup(..))
import Data.Monoid                      (Monoid(..))

---------------------------------------------------------------------------------------------------
data Support n
        = Support
        { -- | Type constructors used in the expression.
          supportTyCon          :: Set (Bound n)

          -- | Type constructors used in the argument of a value-type application.
        , supportTyConXArg      :: Set (Bound n)

          -- | Free spec variables in an expression.
        , supportSpVar          :: Set (Bound n)

          -- | Type constructors used in the argument of a value-type application.
        , supportSpVarXArg      :: Set (Bound n)

          -- | Free witness variables in an expression.
          --   (from the Witness universe)
        , supportWiVar          :: Set (Bound n)

          -- | Free value variables in an expression.
          --   (from the Data universe)
        , supportDaVar          :: Set (Bound n)

          -- | Data constructors used in an expression.
        , supportDaCon          :: Set (DaConBoundName n) }
        deriving Show


-- | Construct an empty support.
emptySupport :: Support n
emptySupport
        = Support
        { supportTyCon          = Set.empty
        , supportTyConXArg      = Set.empty
        , supportSpVar          = Set.empty
        , supportSpVarXArg      = Set.empty
        , supportWiVar          = Set.empty
        , supportDaVar          = Set.empty
        , supportDaCon          = Set.empty }


-- | Union two support records.
unionSupport :: Ord n => Support n -> Support n -> Support n
unionSupport sp1 sp2
        = Support
        { supportTyCon          = Set.unions [supportTyCon sp1,     supportTyCon sp2]
        , supportTyConXArg      = Set.unions [supportTyConXArg sp1, supportTyConXArg sp2]
        , supportSpVar          = Set.unions [supportSpVar sp1,     supportSpVar sp2]
        , supportSpVarXArg      = Set.unions [supportSpVarXArg sp1, supportSpVarXArg sp2]
        , supportWiVar          = Set.unions [supportWiVar sp1,     supportWiVar sp2]
        , supportDaVar          = Set.unions [supportDaVar sp1,     supportDaVar sp2]
        , supportDaCon          = Set.unions [supportDaCon sp1,     supportDaCon sp2] }


instance Ord a => Semigroup (Support a) where
 (<>)           = unionSupport


instance Ord a => Monoid (Support a) where
 mempty         = emptySupport
 mappend        = unionSupport


---------------------------------------------------------------------------------------------------
-- | Get a description of the type and value environment from a Support.
--   Type (level-1) variables are tagged with True, while
--   value and witness (level-0) variables are tagged with False.
supportEnvFlags
        :: Ord n => Support n
        -> Set (Bool, Bound n)

supportEnvFlags supp
 = let
        us1   = Set.map  (\u -> (True,  u)) $ supportSpVar supp

        us0   = Set.unions
                [ Set.map  (\u -> (False, u)) $ supportDaVar supp
                , Set.map  (\u -> (False, u)) $ supportWiVar supp]

   in   Set.union us1 us0


---------------------------------------------------------------------------------------------------
class SupportX (c :: * -> *) where
 support
        :: Ord n
        => KindEnv n -> TypeEnv n
        -> c n
        -> Support n


instance SupportX Type where
 support kenv _tenv t
  = let (fvs1, tcs)     = freeVarConT kenv t
    in  mempty  { supportTyCon  = tcs
                , supportSpVar  = fvs1 }


---------------------------------------------------------------------------------------------------
instance SupportX (Module a) where
 support kenv tenv mm
  = let kenv'   = Env.union kenv (moduleKindEnv mm)
        tenv'   = Env.union tenv (moduleTypeEnv mm)
    in  support kenv' tenv' (moduleBody mm)


-- | Compute the support set of an `ImportCap`.
supportOfImportCap
        :: Ord n
        => KindEnv n -> TypeEnv n
        -> ImportCap n (Type n) -> Support n

supportOfImportCap kenv tenv ic
 = case ic of
        ImportCapAbstract t
         -> support kenv tenv t


-- | Compute the support set of an `ImportValue`.
supportOfImportValue
        :: Ord n
        => KindEnv n -> TypeEnv n
        -> ImportValue n (Type n) -> Support n

supportOfImportValue kenv tenv iv
 = case iv of
        ImportValueModule _mn _n t _arity
         -> support kenv tenv t

        ImportValueSea _mn _n _tx t
         -> support kenv tenv t


---------------------------------------------------------------------------------------------------
instance SupportX (Exp a) where
 support kenv tenv xx
  = case xx of
        XVar _ u
         | Env.member u tenv    -> mempty
         | otherwise            -> mempty { supportDaVar = Set.singleton u}

        XAbs _ (MType b) x
         -> support kenv tenv b
         <> support (Env.extend b kenv) tenv x

        XAbs _ (MTerm b) x
         -> support kenv tenv b
         <> support kenv (Env.extend b tenv) x

        XAbs _ (MImplicit b) x
         -> support kenv tenv b
         <> support kenv (Env.extend b tenv) x

        XApp _ x1 x2
         -> let s1              = support kenv tenv x1
                s2              = support kenv tenv x2
            in  mappend s1 s2

        XLet _a lts x2
         -> let s1              = support kenv tenv lts
                (bs1, bs0)      = bindsOfLets lts
                kenv'           = Env.extends bs1 kenv
                tenv'           = Env.extends bs0 tenv
                s2              = support kenv' tenv' x2
            in  mappend s1 s2

        XAtom _ MAPrim{}        -> mempty

        XAtom _ MALabel{}       -> mempty

        XAtom _ (MACon dc)
         -> case dc of
                DaConBound n    -> mempty { supportDaCon = Set.singleton n }
                _               -> mempty

        XCase _ x1 alts
         -> let s1              = support kenv tenv x1
                ss              = mconcat $ map (support kenv tenv) alts
            in  mappend s1 ss

        XCast _ c1 x2
         -> let s1              = support kenv tenv c1
                s2              = support kenv tenv x2
            in  mappend s1 s2

        XAsync _ b e1 e2
         -> let e1'   = support kenv tenv e1
                b'    = support kenv tenv b
                tenv' = Env.extend b tenv
                e2'   = support kenv tenv' e2
            in e1' <> b' <> e2'

instance SupportX (Arg a) where
 support kenv tenv aa
  = case aa of
        RType t
         -> let sup = support kenv tenv t
            in  sup { supportTyConXArg  = supportTyCon sup
                    , supportSpVarXArg  = supportSpVar sup }

        RTerm x         -> support kenv tenv x
        RWitness  w     -> support kenv tenv w
        RImplicit x     -> support kenv tenv x


instance SupportX (Alt a) where
 support kenv tenv aa
  = case aa of
        AAlt PDefault x
         -> support kenv tenv x

        AAlt (PData _dc bs0) x
         -> let tenv'   = Env.extends bs0 tenv
            in  support kenv tenv' x


instance SupportX (Witness a) where
 support kenv tenv ww
  = case ww of
        WVar _ u
         | Env.member u tenv    -> mempty
         | otherwise            -> mempty { supportWiVar = Set.singleton u }

        WCon{}
         -> mempty

        WApp _ w1 w2
         -> support kenv tenv w1
         <> support kenv tenv w2

        WType _ t
         -> support kenv tenv t


instance SupportX (Cast a) where
 support kenv tenv cc
  = case cc of
        CastWeakenEffect eff    -> support kenv tenv eff
        CastPurify w            -> support kenv tenv w
        CastBox                 -> mempty
        CastRun                 -> mempty


instance SupportX (Lets a) where
 support kenv tenv lts
  = case lts of
        LLet b x
         -> support kenv tenv b
         <> support kenv (Env.extend b tenv) x

        LRec bxs
         -> (mconcat $ map (support kenv tenv) $ map fst bxs)
         <> (let tenv' = Env.extends (map fst bxs) tenv
             in  mconcat $ map (support kenv tenv') $ map snd bxs)

        LPrivate bs t2 ws
         -> (mconcat $ map (support kenv tenv) bs)
         <> (mconcat $ map (support kenv tenv) $ maybeToList t2)
         <> (let kenv' = Env.extends bs kenv
             in  mconcat $ map (support kenv' tenv) ws)


instance SupportX Bind where
 support kenv tenv b
  = support kenv tenv
  $ typeOfBind b


