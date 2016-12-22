
module DDC.Core.Check.Context.Implicit
        ( findImplicitOfType)
where
import DDC.Type.Exp.Simple
import DDC.Core.Check.Context.Base
import DDC.Core.Check.Context.Elem
import DDC.Core.Check.Context.Apply
import qualified Data.Set               as Set


-- | Try to find a term binder in the context that has a type equivalent
--   to the given one.
findImplicitOfType 
        :: Ord n
        => Context n            -- ^ Context to search.
        -> Type    n            -- ^ Type of the binder we want.
        -> Maybe   (Bind n)

findImplicitOfType ctx tWanted
 = search (contextElems ctx)

 where  search []
         = Nothing

        search (e : es)
         = case e of
            ElemPos{}        -> search es
            ElemKind{}       -> search es
            ElemExistsDecl{} -> search es
            ElemExistsEq{}   -> search es

            ElemType b 
             -> case applyContextEither ctx Set.empty (typeOfBind b) of
                 Left{}             -> search es
                 Right tBind_ctx    
                  | equivT (contextEnvT ctx) tBind_ctx tWanted   
                                    -> Just b
                  | otherwise       -> search es

