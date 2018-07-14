
module DDC.Core.Transform.Resolve.Build where
import DDC.Core.Transform.Resolve.Context
import DDC.Core.Transform.Resolve.Base
import DDC.Type.Transform.Instantiate
import DDC.Type.Transform.SubstituteT
import DDC.Type.Transform.Unify
import DDC.Core.Exp.Annot
import DDC.Core.Codec.Text.Pretty
-- import qualified Data.Map.Strict                as Map



-- | Try to build a term of the given type,
--   out of the terms available in the context.
contextResolve
        :: (Ord n, Pretty n, Show n)
        => a
        -> Context n
        -> Type n
        -> S a n (Exp a n)

contextResolve !a !ctx !tWant
 = searchStack (contextBinds ctx)
 where
        -- Search the module-local binding stack first.
        --  If that doesn't work then search the top-level namespace.
        searchStack []
         = searchImports [] -- (contextTop ctx)

        searchStack (g : gs)
         = do   result <- searchGroup g
                case result of
                 Nothing -> searchStack gs
                 Just x  -> return x


        searchGroup []
         = return Nothing

        searchGroup ((nBind, tBind) : nts)
         = do   let match = matchBind a (contextEnvT ctx) tWant nBind tBind
                case match of
                 Nothing
                  -> searchGroup nts

                 Just (xx', tsArg)
                  -> do
                        xsArg   <- mapM (contextResolve a ctx) tsArg
                        return  $ Just
                                $ xApps a xx'
                                $ map (RImplicit . RTerm) xsArg


        -- Search the top-level imported things.
        searchImports []
         = throwE $ ErrorCannotResolve tWant

        searchImports ((nBind, i) : nisMore)
         = do
                let tBind
                        = case i of
                           ImportValueModule{} -> importValueModuleType i
                           ImportValueSea{}    -> importValueSeaType    i

                let match = matchBind a (contextEnvT ctx) tWant nBind tBind
                case match of
                 Nothing
                  -> searchImports nisMore

                 Just (xx', tsArg)
                  -> do xsArg   <- mapM (contextResolve a ctx) tsArg
                        return  $ xApps a xx'
                                $ map (RImplicit . RTerm) xsArg


matchBind
        :: Ord n
        => a
        -> EnvT n               -- ^ Type environment.
        -> Type n               -- ^ Wanted type.
        -> n                    -- ^ Name of binding in environment.
        -> Type n               -- ^ Type of binding.
        -> Maybe (Exp a n, [Type n])

matchBind a envt tWant' nBind  tBind'
 = do
        let match = matchScheme envt tWant' tBind'
        case match of
         Nothing
          ->    Nothing

         Just (tsInst, tsArg)
          ->    Just    ( xApps a (XVar a (UName nBind)) $ map RType tsInst
                        , tsArg)


-- | Match a wanted type against an available scheme.
matchScheme
        :: Ord n
        => EnvT n               -- ^ Type environment.
        -> Type n               -- ^ Wanted type.
        -> Type n               -- ^ Available scheme.
        -> Maybe ( [Type n]     --   Type  arguments to instantiate scheme.
                 , [Type n])    --   Types of more implicit term arguments.

matchScheme envt tWanted tBind
 -- The type of this binding is exactly what we want.
 | equivT envt tWanted tBind
 = Just ([], [])

 -- Check if the binding
 | otherwise
 = let
        -- Split off any type parameters.
        (bsParamType, _tBody)
         = case takeTForalls tBind of
                Just (bs, t)    -> (bs, t)
                Nothing         -> ([], tBind)

        -- Instantiate the type with new existentials.
        nArgs   = length bsParamType
        tsArgExists
                = [ TCon (TyConExists i k)
                  | i <- [0..]
                  | k <- map typeOfBind bsParamType]

        Just tBind_inst
                = instantiateTs tBind tsArgExists

        -- Split of any implicit value parameters.
        (tsParamTerm, tResult)
                = takeTFunImplicits tBind_inst

        result  = unifyExistsRight envt tWanted tResult

        -- Try to unify the wanted type with the result we
        -- would get if we applied the function.
   in   case result of
         Just cs
          |  Just tsArgInst
                <- sequence
                $  map (\i -> Prelude.lookup i cs)
                $ [0.. nArgs - 1]

          -> let tsParamTerm' = map (substituteExistsT cs) tsParamTerm
             in  Just (tsArgInst, tsParamTerm')

         _ -> Nothing


