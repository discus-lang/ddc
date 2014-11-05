
module DDC.Core.Tetra.Convert.Exp.Base
        ( Convert
        , Config        (..)

        -- * Context
        , Context       (..)
        , extendKindEnv, extendsKindEnv
        , extendTypeEnv, extendsTypeEnv

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
-- | Context of the conversion.
data Context 
        = Context
        { -- | The platform that we're converting to, 
          --   this sets the pointer width.
          contextPlatform       :: Platform

          -- | Data type definitions.
        , contextDataDefs       :: DataDefs E.Name

          -- | Types of imported values.
        , contextImports        :: Set      E.Name

          -- | Names of supers that are directly callable.
        , contextSupers         :: Set      E.Name

          -- | Current kind environment.
        , contextKindEnv        :: KindEnv  E.Name

          -- | Current type environment.
        , contextTypeEnv        :: TypeEnv  E.Name 

          -- Functions to convert the various parts of the AST.
          -- We tie the recursive knot though this Context type so that
          -- we can split the implementation into separate non-recursive modules.
        , contextConvertExp
                :: forall a. Show a => ExpContext -> Convert a Exp 

        , contextConvertLets    
                :: forall a. Show a => Convert a Lets

        , contextConvertAlt     
                :: forall a. Show a 
                => a
                -> Bound E.Name -> Type E.Name
                -> ExpContext   -> Convert a Alt  }



extendKindEnv  ::  Bind E.Name  -> Context -> Context
extendKindEnv b ctx
        = ctx { contextKindEnv = Env.extend b (contextKindEnv ctx) }

extendsKindEnv :: [Bind E.Name] -> Context -> Context
extendsKindEnv bs ctx
        = ctx { contextKindEnv = Env.extends bs (contextKindEnv ctx) }


extendTypeEnv  :: Bind E.Name   -> Context -> Context
extendTypeEnv b ctx
        = ctx { contextTypeEnv = Env.extend b (contextTypeEnv ctx) }

extendsTypeEnv :: [Bind E.Name] -> Context -> Context
extendsTypeEnv bs ctx
        = ctx { contextTypeEnv = Env.extends bs (contextTypeEnv ctx) }


---------------------------------------------------------------------------------------------------
type Convert a c
        =  Context
        -> c (AnTEC a E.Name)   E.Name
        -> ConvertM a (c a A.Name)

data Config a
        = Config
        { configConvertExp      :: Convert a Exp
        , configConvertAlt      :: Convert a Alt
        , configConvertLets     :: Convert a Lets }



---------------------------------------------------------------------------------------------------
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


-- | Get the value arity of a supercombinator. 
--   This is how many data arguments it needs when we call it.
superDataArity :: Context -> Bound E.Name -> Maybe Int
superDataArity ctx u
        | UName n  <- u
        , Just  t  <- Env.lookup u (contextTypeEnv ctx)
        , Set.member n (contextSupers ctx)
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
