
module DDC.Core.Tetra.Transform.Curry
        (curryModule)
where
import DDC.Type.Env                             (KindEnv, TypeEnv)
import DDC.Core.Annot.AnTEC
import DDC.Core.Tetra
import DDC.Core.Tetra.Compounds
import DDC.Core.Predicates
import DDC.Core.Module
import DDC.Core.Exp
import Data.Maybe
import Data.List                                (foldl')
import Data.Map                                 (Map)
import qualified DDC.Type.Env                   as Env
import qualified Data.Map                       as Map


-- TODO: handle supers names being shadowed by local bindings.
-- TODO: ensure type lambdas are out the front of supers, supers in prenex form.
-- TODO: build thunks for partially applied foreign functions.
-- TODO: handle monomorphic functions being passed to contructors, etc.
--       not an app but we need to build a closure.
-- TOOD: also handle under/over applied data constructors, do a transform
--       beforehand to saturate them.


-- | Insert primitives to manage higher order functions in a module.
curryModule 
        :: Show a
        => Module (AnTEC a Name) Name -> Module (AnTEC a Name) Name

curryModule mm
 = let  
        -- Add all the foreign functions to the function map.
        -- We can do a saturated call for these directly.
        funs_foreign
                = foldl' funMapAddForeign Map.empty
                $ moduleImportValues mm

        -- Apply curry transform in the body of the module.
        xBody'  = curryBody (funs_foreign, Env.empty, Env.empty)
                $ moduleBody mm

   in   mm { moduleBody = xBody' }


---------------------------------------------------------------------------------------------------
-- | Map of functional values to their types and arities.
type FunMap
        = Map Name Fun

-- | Enough information about a functional thing to decide how we 
--   should call it. 
data Fun
        -- | A locally defined top-level supercombinator. 
        --   We can do a saturated call for these directly.
        --   The arity of the super can be determined by inspecting the
        --   definition in the current module.
        = FunLocalSuper
        { _funName      :: Name
        , _funType      :: Type Name 
        , _funArity     :: Int }

        | FunExternSuper
        { _funName      :: Name
        , _funType      :: Type Name
        , _funArity     :: Int }

        -- | A foreign imported function.
        --   We can do a saturated call for these directly.
        --   Foreign functions are not represented as closures, 
        --   so we can determine their arity directly from their types.
        | FunForeignSea
        { _funName      :: Name
        , _funType      :: Type Name
        , _funArity     :: Int }
        deriving Show


-- | Add the type of this binding to the function map.
funMapAddLocalSuper :: FunMap -> (Bind Name, Exp a Name) -> FunMap
funMapAddLocalSuper funs (b, x)
        | BName n t             <- b
        = let   -- Get the value arity of the super, that is, how many
                -- values we need to saturate all the value lambdas.
                (flags, _) = fromMaybe ([], x) (takeXLamFlags x)
                arity      = length $ filter (== False) $ map fst flags

          in    Map.insert n (FunLocalSuper n t arity) funs

        | otherwise
        = funs


-- | Add the type of a foreign import to the function map.
funMapAddForeign :: FunMap -> (Name, ImportValue Name) -> FunMap
funMapAddForeign funs (n, is)

        -- Import from a different DDC compiled module.
        | ImportValueModule _m _n t _ <- is
        = let   (tsArgs, _tResult)                      -- TODO: get real arity of function.
                        = takeTFunArgResult
                        $ eraseTForalls t

                arity   = length tsArgs

          in    Map.insert n (FunExternSuper n t arity) funs


        -- Import from a Sea land.
        | ImportValueSea _ t  <- is
        = let   (tsArgs, _tResult)
                        = takeTFunArgResult
                        $ eraseTForalls t

                arity   = length tsArgs

          in    Map.insert n (FunForeignSea n t arity) funs


        | otherwise
        = funs


---------------------------------------------------------------------------------------------------
type Context
        = (FunMap, KindEnv Name, TypeEnv Name)

-- | Manage higher-order functions in a module body.
curryBody
        :: Show a
        => Context
        -> Exp (AnTEC a Name) Name
        -> Exp (AnTEC a Name) Name

curryBody (funs, kenv, tenv) xx
 = case xx of
        XLet a (LRec bxs) x2
         -> let (bs, xs) = unzip bxs

                -- Add types of supers to the function map.
                funs'   = foldl funMapAddLocalSuper funs bxs

                -- The new type environment.
                tenv'   = Env.extends bs tenv

                -- Rewrite bindings in the body of the let-expression.
                ctx'    = (funs', kenv, tenv')
                xs'     = map (curryX ctx') xs
                bxs'    = zip bs xs'

            in  XLet a (LRec bxs') 
                 $ curryBody ctx' x2

        _ -> xx


---------------------------------------------------------------------------------------------------
curryX  :: Show a
        => Context
        -> Exp (AnTEC a Name) Name 
        -> Exp (AnTEC a Name) Name

curryX ctx@(funs, _kenv, _tenv) xx
 = let down    x   = curryX   ctx x
   in  case xx of
        XVar a u
         | UName nF             <- u
         -> makeCall xx a funs nF []

         | otherwise
         -> xx

        XCon{}          -> xx
        XLam a b x      -> XLam a b (down x)
        XLAM a b x      -> XLAM a b (down x)

        XApp a x1 x2
         -- If this is an explicit use of the creify# op,
         -- then don't reify the function again.
         | Just (xF, [XType{}, XType{}, _]) <- takeXApps xx
         , XVar _ (UPrim nF _)    <- xF
         , NameOpFun OpFunCReify  <- nF
         -> xx

         -- Decode how to call this function.
         | Just (xF, xsArgs)    <- takeXApps xx
         , XVar a' (UName nF)   <- xF
         , length xsArgs  > 0
         -> let xsArgs' = map down xsArgs
            in  makeCall xx a' funs nF xsArgs'

         -- If the functional value is not a named variable then 
         -- just pass it though. 
         | otherwise
         -> XApp a (down x1) (down x2)

        XLet  a lts x   -> XLet  a   (curryLts ctx lts) (down x)
        XCase a x alts  -> XCase a   (down x) (map (curryAlt ctx) alts)
        XCast a c x     -> XCast a c (down x)
        XType{}         -> xx
        XWitness{}      -> xx


-- | Manage function application in a let binding.
curryLts :: Show a
        => Context
        -> Lets (AnTEC a Name) Name -> Lets (AnTEC a Name) Name

curryLts ctx lts
 = case lts of
        LLet b x        -> LLet b (curryX ctx x)
        LRec bxs        -> LRec [(b, curryX ctx x) | (b, x) <- bxs]
        LPrivate{}      -> lts
        LWithRegion{}   -> lts


-- | Manage function application in a case alternative.
curryAlt :: Show a
        => Context
        -> Alt (AnTEC a Name) Name  -> Alt (AnTEC a Name) Name

curryAlt ctx alt
 = case alt of
        AAlt w x        -> AAlt w (curryX ctx x)


---------------------------------------------------------------------------------------------------
-- | Call a thing, depending on what it is.
--   Decide how to call the functional thing, depending on 
--   whether its a super, foreign imports, or thunk.
makeCall
        :: Show a
        => Exp (AnTEC a Name) Name
        -> AnTEC a Name         -- ^ Annotation from functional part of application.
        -> FunMap               -- ^ Types and arities of functions in the environment.
        -> Name                 -- ^ Name of function to call.
        -> [Exp (AnTEC a Name) Name]    -- ^ Arguments to function.
        ->  Exp (AnTEC a Name) Name

makeCall xx aF funMap nF xsArgs
        -- Call a top-level super in the local module.
        | Just (tF, iArity) 
            <- case Map.lookup nF funMap of
                Just (FunLocalSuper  _ tF iArity)       -> Just (tF, iArity)
                Just (FunExternSuper _ tF iArity)       -> Just (tF, iArity)
                Just (FunForeignSea  _ tF iArity)       -> Just (tF, iArity)
                _                                       -> Nothing
        
        -- split the quantifiers from the type of the super.
        , (bsForall, tBody)                  <- fromMaybe ([], tF) $ takeTForalls tF
        
        -- split the body type into value parameters and result.
        , (tsParam, tResult)                 <- takeTFunArgResult tBody
        
        -- split the value parameters into ones accepted by lambdas and ones that 
        -- are accepted by the returned closure.
        , (tsParamLam, tsParamClo)           <- splitAt iArity tsParam
        
        -- build the type of the returned value.
        , Just tResult'                      <- tFunOfList (tsParamClo ++ [tResult])
        
        -- split the arguments into the type arguments that satisfy the quantifiers,  
        -- then the value arguments.
        , Just (xsArgTypes, xsArgValues)     <- splitAppsValues xsArgs
        
        -- there must be types to satisfy all of the quantifiers
        , length bsForall == length xsArgTypes
 
        = makeCallSuper aF nF
                (xApps aF (XVar aF (UName nF)) xsArgTypes)
                tsParamLam 
                tResult' 
                xsArgValues

        -- | Apply a thunk to its arguments.
        | length xsArgs > 0
        = makeCallThunk aF nF xsArgs

        -- | This was an existing thunk applied to no arguments,
        --   so we can just return it without doing anything.
        | otherwise
        = xx


---------------------------------------------------------------------------------------------------
-- | Call a top-level supercombinator,
--   or foriegn function imported from Sea land.
makeCallSuper 
        :: Show a 
        => AnTEC a Name                 -- ^ Annotation to use.
        -> Name                         -- ^ Name of super to call.
        -> Exp  (AnTEC a Name) Name     -- ^ Expression of super to call.
        -> [Type Name]                  -- ^ Parameter types of super
        -> Type Name                    -- ^ Return type of super.
        -> [Exp (AnTEC a Name) Name]    -- ^ Arguments to super.
        -> Exp  (AnTEC a Name) Name

makeCallSuper aF _nF xF tsParamLam tResultSuper xsArgs
 -- Fully saturated call to a super of foreign function. 
 -- We have arguments for each parameter, so can call it directly.
 | length xsArgs == length tsParamLam
 = -- trace ("sat " ++ show (nF, length tsParamLam, length xsArgs)) $
   xApps aF xF xsArgs

 -- Partially application of a super or foreign function.
 -- We need to build a closure, then attach any arguments we have.
 | length xsArgs   <  length tsParamLam
 = let 
        -- Types of the applied arguments.
{-        tsArgs  = map annotType     
                $ map annotOfExp xsArgs
-}
        -- Split types of the super parameters into the ones that can be
        -- satisfied by this application, and the remaining parameters that
        -- are still waiting for arguments.
        (tsParamSat, tsParamRemain)     
                = splitAt (length xsArgs) tsParamLam

        -- The type of the result after performing this application.
        -- If there are remaining, un-saturated parameters the result
        -- type will still be a function.
        Just tResultClo
                = tFunOfList (tsParamRemain ++ [tResultSuper])

        tParamFirst : tsParamRest       = tsParamLam
        Just tSuperResult               = tFunOfList (tsParamRest ++ [tResultSuper])

   in  -- trace ("pap " ++ show (nF, length tsParamLam, length xsArgs)) $
          xApps aF (xFunCurry aF tsParamSat tResultClo 
                           (xFunCReify aF tParamFirst tSuperResult xF))
                       xsArgs

 -- TODO: handle over-applied super.
 --       do direct call, then do an application.
 --       the super must produce a closure, otherwise it won't be well typed.
 | otherwise
 = -- trace ("ovr " ++ show (nF, length tsParamLam, length xsArgs)) $
   xApps aF xF xsArgs


-- | Given a list of expressions, consisting of some XTypes then some
--   non-XType expressoins, split it into the XTypes then the values.
--   If there are any more trailing XTypes then Nothing.
splitAppsValues :: [Exp a Name] -> Maybe ([Exp a Name], [Exp a Name])
splitAppsValues xs
 = let  (xsT, xsMore)   = span isXType xs
        (xsV, xsMore2)  = span (not . isXType) xsMore
   in   if null xsMore2 
         then Just (xsT, xsV)
         else Nothing


---------------------------------------------------------------------------------------------------
-- | Apply a thunk to some more arguments.
makeCallThunk
        :: Show a
        => AnTEC a Name                 -- ^ Annotation from functional part of application.
        -> Name                         -- ^ Name of thunk.
        -> [Exp (AnTEC a Name) Name]    -- ^ Arguments to thunk.
        ->  Exp (AnTEC a Name) Name

makeCallThunk aF nF xsArgs
 = let  tsArgs          = map annotType $ map annotOfExp xsArgs
        (_, tResult)    = takeTFunArgResult $ annotType aF
   in   xFunApply aF tsArgs tResult (XVar aF (UName nF)) xsArgs

