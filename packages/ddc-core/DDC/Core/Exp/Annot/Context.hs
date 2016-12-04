
module DDC.Core.Exp.Annot.Context
        ( Context (..)
        , enterLAM
        , enterLam
        , enterAppLeft
        , enterAppRight
        , enterLetBody
        , enterLetLLet
        , enterLetLRec
        , enterCaseScrut
        , enterCaseAlt
        , enterCastBody)
where
import DDC.Core.Exp.Annot.Exp
import DDC.Core.Exp.Annot.Ctx
import DDC.Core.Exp.Annot.Compounds
import DDC.Core.Env.EnvX                (EnvX)
import qualified DDC.Core.Env.EnvX      as EnvX

data Context a n
        = Context
        { contextEnv    :: EnvX n
        , contextCtx    :: Ctx a n }


-- | Enter the body of a type lambda.
enterLAM 
        :: Ord n => Context a n
        -> a -> Bind n -> Exp a n
        -> (Context a n -> Exp a n -> b) -> b

enterLAM c a b x f
 = let  c' = c  { contextEnv    = EnvX.extendT b (contextEnv c)
                , contextCtx    = CtxLAM (contextCtx c) a b }
   in   f c' x


-- | Enter the body of a value lambda.
enterLam
        :: Ord n => Context a n
        -> a -> Bind n -> Exp a n
        -> (Context a n -> Exp a n -> b) -> b

enterLam c a b x f
 = let  c' = c  { contextEnv    = EnvX.extendX b (contextEnv c) 
                , contextCtx    = CtxLam (contextCtx c) a b }
   in   f c' x


-- | Enter the left of an application.
enterAppLeft   
        :: Context a n
        -> a -> Exp a n -> Arg a n
        -> (Context a n -> Exp a n -> b) -> b

enterAppLeft c a x1 x2 f
 = let  c' = c  { contextCtx     = CtxAppLeft (contextCtx c) a x2 }
   in   f c' x1


-- | Enter the right of an application.
enterAppRight
        :: Context a n
        -> a -> Exp a n -> Arg a n
        -> (Context a n -> Arg a n -> b) -> b

enterAppRight c a x1 a2 f
 = let  c' = c  { contextCtx    = CtxAppRight (contextCtx c) a x1 }
   in   f c' a2


-- | Enter the body of a let-expression.
enterLetBody
        :: Ord n => Context a n 
        -> a -> Lets a n -> Exp a n
        -> (Context a n -> Exp a n -> b) -> b

enterLetBody c a lts x f
 = let  (bs1, bs0) = bindsOfLets lts
        c' = c  { contextEnv    = EnvX.extendsX bs0 
                                $ EnvX.extendsT bs1
                                $ contextEnv c
                , contextCtx      = CtxLetBody (contextCtx c) a lts }
   in   f c' x


-- | Enter the binding of a LLet
enterLetLLet
        :: Context a n
        -> a -> Bind n -> Exp a n -> Exp a n
        -> (Context a n -> Exp a n -> b) -> b

enterLetLLet c a b x xBody f
 = let  c' = c  { contextCtx    = CtxLetLLet (contextCtx c) a b xBody }
   in   f c' x


-- | Enter a binding of a LRec group.
enterLetLRec
        :: Ord n => Context a n
        -> a -> [(Bind n, Exp a n)] -> Bind n -> Exp a n -> [(Bind n, Exp a n)] -> Exp a n
        -> (Context a n -> Exp a n -> b) -> b

enterLetLRec c a bxsBefore b x bxsAfter xBody f
 = let  bsBefore = map fst bxsBefore
        bsAfter  = map fst bxsAfter
        c' = c  { contextEnv    = EnvX.extendsX (bsBefore ++ [b] ++ bsAfter)
                                        (contextEnv c) 
                , contextCtx    = CtxLetLRec (contextCtx c) a 
                                        bxsBefore b bxsAfter xBody 
                }
   in   f c' x


-- | Enter the scrutinee of a case-expression.
enterCaseScrut
        :: Context a n
        -> a -> Exp a n -> [Alt a n]
        -> (Context a n -> Exp a n -> b) -> b

enterCaseScrut c a x alts f
 = let  c' = c  { contextCtx     = CtxCaseScrut (contextCtx c) a alts }
   in   f c' x


-- | Enter the right of an alternative.
enterCaseAlt 
        :: Ord n => Context a n
        -> a -> Exp a n -> [Alt a n] -> Pat n -> Exp a n -> [Alt a n]
        -> (Context a n -> Exp a n -> b) -> b

enterCaseAlt c a xScrut altsBefore w x altsAfter f
 = let  bs      = bindsOfPat w
        c' = c  { contextEnv    = EnvX.extendsX bs (contextEnv c)
                , contextCtx    = CtxCaseAlt (contextCtx c) a
                                        xScrut altsBefore w altsAfter }
   in   f c' x


-- | Enter the body of a cast
enterCastBody
        :: Context a n
        -> a -> Cast a n -> Exp a n
        -> (Context a n -> Exp a n -> b) -> b

enterCastBody c a cc x f
 = let  c' = c  { contextCtx    = CtxCastBody (contextCtx c) a cc }
   in   f c' x
