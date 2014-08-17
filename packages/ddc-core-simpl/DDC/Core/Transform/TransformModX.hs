
-- | Helper for transforming the bindings in a module
module DDC.Core.Transform.TransformModX
        ( transformModX
        , transformModLet
        )
where
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Core.Compounds

import Control.Arrow


-- | Apply transform to each expression let binding in module
transformModX   :: (Exp a n -> Exp a n)
                -> Module a n
                -> Module a n
transformModX f mm
 = transformModLet (const f) mm


-- | Apply transform to each expression let binding in module, with bind too
transformModLet :: (Bind n -> Exp a n -> Exp a n)
                -> Module a n
                -> Module a n
transformModLet f mm
 = let body      = moduleBody mm

       (lets,xx) = splitXLetsAnnot body
       lets'     = map (first go) lets

       body'     = xLetsAnnot lets' xx

   in  mm { moduleBody = body' }
 where
  go (LRec binds)
   = LRec [ (b, f b x)
          | (b, x) <- binds]

  go (LLet b x)
   = LLet   b (f b x)

  go  l
   = l

