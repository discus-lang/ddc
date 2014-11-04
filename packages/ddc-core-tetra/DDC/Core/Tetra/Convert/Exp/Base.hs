
module DDC.Core.Tetra.Convert.Exp.Base
        ( Convert
        , Config        (..)

        -- * Context
        , Context       (..)
        , TopEnv        (..)
        , ExpContext    (..)
        , superDataArity

        -- * Constructors
        , xConvert
        , xTakePtr
        , xMakePtr)
where
import DDC.Core.Tetra.Convert.Error
import DDC.Core.Salt.Platform
import DDC.Core.Compounds
import DDC.Core.Exp
import DDC.Core.Check                    (AnTEC(..))
import DDC.Type.DataDef
import DDC.Type.Env                      (KindEnv, TypeEnv)
import Data.Set                          (Set)
import qualified DDC.Core.Tetra.Prim     as E
import qualified DDC.Type.Env            as Env
import qualified DDC.Core.Salt.Name      as A
import qualified DDC.Core.Salt.Env       as A
import qualified Data.Set                as Set


---------------------------------------------------------------------------------------------------
type Convert a c
        =  Config a 
        -> TopEnv 
        -> KindEnv  E.Name 
        -> TypeEnv  E.Name 
        -> c (AnTEC a E.Name)   E.Name
        -> ConvertM a (c a A.Name)

data Config a
        = Config
        { configConvertExp      :: Convert a Exp
        , configConvertAlt      :: Convert a Alt
        , configConvertLets     :: Convert a Lets }


---------------------------------------------------------------------------------------------------
data Context 
        = Context
        { contextTopEnv         :: TopEnv
        , contextKindEnv        :: KindEnv E.Name
        , contextTypeEnv        :: TypeEnv E.Name }


-- | The context we're converting an expression in.
--   We keep track of this during conversion to ensure we don't produce
--   code outside the Salt language fragment. For example, in Salt a function
--   can only be applied to a value variable, type or witness -- and not
--   a general expression.
data ExpContext
        = ExpTop        -- ^ At the top-level of the module.
        | ExpFun        -- ^ At the top-level of a function.
        | ExpBody       -- ^ In the body of a function.
        | ExpBind       -- ^ In the right of a let-binding.
        | ExpArg        -- ^ In a function argument.
        deriving (Show, Eq, Ord)


-- | Information about the top-level environment.
data TopEnv
        = TopEnv
        { -- Platform we're converting to.
          topEnvPlatform        :: Platform

          -- Data type definitions.
        , topEnvDataDefs        :: DataDefs E.Name

          -- Names of top-level supercombinators that are directly callable.
        , topEnvSupers          :: Set E.Name 

          -- Names of imported values that can be refered to directly.
        , topEnvImportValues    :: Set E.Name }


-- | Get the value arity of a supercombinator. 
--   This is how many data arguments it needs when we call it.
superDataArity :: TopEnv -> TypeEnv E.Name -> Bound E.Name -> Maybe Int
superDataArity env tenv u
        | UName n  <- u
        , Just  t  <- Env.lookup u tenv
        , Set.member n (topEnvSupers env)
        = Just $ dataArityOfType t

        | otherwise
        = Nothing



---------------------------------------------------------------------------------------------------
xConvert :: a -> Type A.Name -> Type A.Name -> Exp a A.Name -> Exp a A.Name
xConvert a t1 t2 x1
        = xApps a (XVar a  (UPrim (A.NamePrimOp $ A.PrimCast $ A.PrimCastConvert)
                                  (A.typeOfPrimCast A.PrimCastConvert)))
                  [ XType a t1, XType a t2, x1 ]


xTakePtr :: a -> Type A.Name -> Type A.Name -> Exp a A.Name -> Exp a A.Name
xTakePtr a tR tA x1
        = xApps a (XVar a  (UPrim (A.NamePrimOp $ A.PrimStore A.PrimStoreTakePtr)
                                  (A.typeOfPrimStore A.PrimStoreTakePtr)))
                  [ XType a tR, XType a tA, x1 ]


xMakePtr :: a -> Type A.Name -> Type A.Name -> Exp a A.Name -> Exp a A.Name
xMakePtr a tR tA x1
        = xApps a (XVar a  (UPrim (A.NamePrimOp $ A.PrimStore A.PrimStoreMakePtr)
                                  (A.typeOfPrimStore A.PrimStoreMakePtr)))
                  [ XType a tR, XType a tA, x1 ]
