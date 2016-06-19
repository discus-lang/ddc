
module DDC.Core.Tetra.Convert.Exp.Base
        ( -- * Context
          Context       (..)
        , typeContext
        , extendKindEnv, extendsKindEnv
        , extendTypeEnv, extendsTypeEnv

        , ExpContext    (..)

        -- * Constructors
        , xConvert
        , xTakePtr
        , xMakePtr)
where
import DDC.Core.Tetra.Transform.Curry.Callable
import DDC.Core.Tetra.Convert.Error
import DDC.Core.Salt.Platform
import DDC.Core.Exp.Annot
import DDC.Core.Check                                   (AnTEC(..))
import DDC.Type.DataDef
import DDC.Type.Env                                     (KindEnv, TypeEnv)
import Data.Set                                         (Set)
import Data.Map                                         (Map)
import qualified DDC.Core.Tetra.Convert.Type.Base       as T
import qualified DDC.Core.Tetra.Prim                    as E
import qualified DDC.Type.Env                           as Env
import qualified DDC.Core.Salt.Name                     as A
import qualified DDC.Core.Salt.Env                      as A


---------------------------------------------------------------------------------------------------
-- | Context of an Exp conversion.
data Context a
        = Context
        { -- | The platform that we're converting to, 
          --   this sets the pointer width.
          contextPlatform       :: Platform

          -- | Data type definitions.
          --   All the visible type equations, from the current and imported modules.
        , contextDataDefs       :: DataDefs E.Name

          -- | Type equations.
          --   All the visible type equations, from the current and imported modules.
        , contextTypeEqns       :: Map E.Name (Type E.Name)

          -- | Names of foreign boxed data type contructors.
          --   These are names like 'Ref' and 'Array' that are defined in the
          --   runtime system rather than as an algebraic data type with a 
          --   Tetra-level data type definition. Although there is no data
          --   type definition, we still represent the values of these types
          --   in generic boxed form.
        , contextForeignBoxedTypeCtors 
                                :: Set      E.Name

          -- | Call patterns of things that we can call directly, in the generated code.
          --   This is locally defined supers, as well as imported supers and sea functions.
        , contextCallable       :: Map E.Name Callable

          -- | Current kind environment.
          --   This is updated as we decend into the AST during conversion.
        , contextKindEnv        :: KindEnv  E.Name

          -- | Current type environment.
          --   This is updated as we decend into the AST during conversion.
        , contextTypeEnv        :: TypeEnv  E.Name 

          -- | Re-bindings of top-level supers.
          --   This is used to handle let-expressions like 'f = g [t]' where
          --   'g' is a top-level super. See [Note: Binding top-level supers]
          --   Maps the left hand variable to the right hand one, eg f -> g,
          --   along with its unpacked type arguments.
        , contextSuperBinds     
                :: Map E.Name (E.Name, [(AnTEC a E.Name, Type E.Name)])

          -- Functions to convert the various parts of the AST.
          -- We tie the recursive knot though this `Context` type so that
          -- we can split the implementation into separate non-recursive modules.
        , contextConvertExp
                :: ExpContext   -> Context a
                -> Exp  (AnTEC a E.Name) E.Name
                -> ConvertM a (Exp a A.Name)

        , contextConvertLets    
                :: Context a
                -> Lets (AnTEC a E.Name)   E.Name
                -> ConvertM a (Maybe (Lets a A.Name), Context a)

        , contextConvertAlt     
                :: a
                -> Bound E.Name -> Type E.Name
                -> ExpContext   -> Context a
                -> Alt  (AnTEC a E.Name) E.Name
                -> ConvertM a (Alt a A.Name)  
        }


-- | Create a type context from an expression context.
typeContext :: Context a -> T.Context
typeContext ctx
        = T.Context
        { T.contextDataDefs     = contextDataDefs ctx
        , T.contextTypeEqns     = contextTypeEqns ctx
        , T.contextForeignBoxedTypeCtors 
                                = contextForeignBoxedTypeCtors ctx
        , T.contextKindEnv      = contextKindEnv  ctx }


-- | Extend the kind environment of a context with a new binding.
extendKindEnv  ::  Bind E.Name  -> Context a -> Context a
extendKindEnv b ctx
        = ctx { contextKindEnv = Env.extend b (contextKindEnv ctx) }


-- | Extend the kind environment of a context with some new bindings.
extendsKindEnv :: [Bind E.Name] -> Context a -> Context a
extendsKindEnv bs ctx
        = ctx { contextKindEnv = Env.extends bs (contextKindEnv ctx) }


-- | Extend the type environment of a context with a new binding.
extendTypeEnv  :: Bind E.Name   -> Context a -> Context a
extendTypeEnv b ctx
        = ctx { contextTypeEnv = Env.extend b (contextTypeEnv ctx) }


-- | Extend the type environment of a context with some new bindings.
extendsTypeEnv :: [Bind E.Name] -> Context a -> Context a
extendsTypeEnv bs ctx
        = ctx { contextTypeEnv = Env.extends bs (contextTypeEnv ctx) }


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


