
module DDC.Core.Exp.AnnotCtx   
        ( Ctx (..)
        , topOfCtx
        , takeEnclosingCtx
        , takeTopNameOfCtx
        , takeTopLetEnvNamesOfCtx
        , encodeCtx)
where
import DDC.Type.DataDef
import DDC.Core.Exp.Annot
import DDC.Type.Env             (KindEnv, TypeEnv)
import Data.Set                 (Set)
import qualified DDC.Type.Env   as Env
import qualified Data.Set       as Set
import qualified Data.Map       as Map


-- | A one-hole context for `Exp`.
data Ctx a n
        -- | The top-level context.
        = CtxTop        !(DataDefs n)
                        !(KindEnv n)
                        !(TypeEnv n)

        -- | Body of a type abstraction.
        | CtxLAM        !(Ctx a n) !a
                        !(Bind n)
        
        -- | Body of a value or witness abstraction.
        | CtxLam        !(Ctx a n) !a
                        !(Bind n)

        -- | Left of an application.
        | CtxAppLeft    !(Ctx a n) !a
                        !(Exp a n)

        -- | Right of an application.
        | CtxAppRight   !(Ctx a n) !a
                        !(Exp a n)

        -- | Body of a let-expression.
        | CtxLetBody    !(Ctx a n) !a
                        !(Lets a n)

        -- | In a non-recursive let-binding.
        --   We store the binder and body of the let expression.
        | CtxLetLLet    !(Ctx a n) !a
                        !(Bind n)       -- binder of current let-binding.
                        !(Exp a n)      -- let body

        -- | In a recursive binding.
        | CtxLetLRec    !(Ctx a n) !a
                        ![(Bind n, Exp a n)] !(Bind n) ![(Bind n, Exp a n)]
                        !(Exp a n)

        -- | Scrutinee of a case expression.
        | CtxCaseScrut  !(Ctx a n) !a
                        ![Alt a n]

        -- | In a case alternative.
        | CtxCaseAlt    !(Ctx a n) !a
                        !(Exp a n)      -- case scrutinee
                        ![Alt a n] !(Pat n) ![Alt a n]

        -- | Body of a type cast
        | CtxCastBody   !(Ctx a n) !a   -- context of let-expression.
                        !(Cast a n)


-- | Get the top level of a context.
topOfCtx :: Ctx a n 
         -> (DataDefs n, KindEnv n, TypeEnv n)

topOfCtx ctx
 = case ctx of
        CtxTop defs kenv tenv    -> (defs, kenv, tenv)
        CtxLAM       c _ _       -> topOfCtx c
        CtxLam       c _ _       -> topOfCtx c
        CtxAppLeft   c _ _       -> topOfCtx c
        CtxAppRight  c _ _       -> topOfCtx c
        CtxLetBody   c _ _       -> topOfCtx c
        CtxLetLLet   c _ _ _     -> topOfCtx c
        CtxLetLRec   c _ _ _ _ _ -> topOfCtx c
        CtxCaseScrut c _ _       -> topOfCtx c
        CtxCaseAlt   c _ _ _ _ _ -> topOfCtx c
        CtxCastBody  c _ _       -> topOfCtx c


-- | Take the enclosing context from a nested one,
--   or `Nothing` if this is the top-level context.
takeEnclosingCtx :: Ctx a n -> Maybe (Ctx a n)
takeEnclosingCtx ctx
 = case ctx of
        CtxTop{}                 -> Nothing
        CtxLAM       c _ _       -> Just c
        CtxLam       c _ _       -> Just c
        CtxAppLeft   c _ _       -> Just c
        CtxAppRight  c _ _       -> Just c
        CtxLetBody   c _ _       -> Just c
        CtxLetLLet   c _ _ _     -> Just c
        CtxLetLRec   c _ _ _ _ _ -> Just c
        CtxCaseScrut c _ _       -> Just c
        CtxCaseAlt   c _ _ _ _ _ -> Just c
        CtxCastBody  c _ _       -> Just c


-- | Take the name of the outer-most enclosing let-binding of this context,
--   if there is one.
takeTopNameOfCtx :: Ctx a n -> Maybe n
takeTopNameOfCtx ctx0
 = eat ctx0
 where  eat ctx
         = case ctx of
                CtxTop{}
                 -> Nothing
                
                CtxLetLLet CtxTop{} _ (BName n _) _
                 -> Just n

                CtxLetLRec CtxTop{} _ _ (BName n _) _ _
                 -> Just n

                _ -> case takeEnclosingCtx ctx of
                        Nothing   -> Nothing
                        Just ctx' -> eat ctx'


-- | Get the set of value names defined at top-level, including top-level 
--   let-bindings and the top level type environment.
takeTopLetEnvNamesOfCtx :: Ord n => Ctx a n -> Set n
takeTopLetEnvNamesOfCtx ctx0
 = eatCtx ctx0
 where  eatCtx ctx
         = case ctx of
                CtxTop _ _ tenv
                 -> Set.fromList
                 $  Map.keys $ Env.envMap tenv

                CtxLetLLet (CtxTop _ _ tenv) _ b xBody
                 -> Set.unions
                        [ Set.fromList $ Map.keys $ Env.envMap tenv
                        , eatBind b
                        , eatExp xBody]

                CtxLetLRec (CtxTop _ _ tenv) _ bxsBefore b bxsAfter xBody
                 -> Set.unions
                        [ Set.fromList  $ Map.keys $ Env.envMap tenv
                        , Set.unions    $ map (eatBind . fst) bxsBefore
                        , eatBind b
                        , Set.unions    $ map (eatBind . fst) bxsAfter
                        , eatExp xBody]

                _ -> case takeEnclosingCtx ctx of
                        Nothing   -> Set.empty
                        Just ctx' -> eatCtx ctx'

        eatExp xx
         = case xx of
                XLet _ (LLet b _) xBody
                 -> Set.unions
                        [ eatBind  b
                        , eatExp xBody ]

                XLet _ (LRec bxs) xBody
                 -> Set.unions
                        [ Set.unions $ map (eatBind . fst) bxs
                        , eatExp xBody ]

                _ -> Set.empty

        eatBind (BName n _) = Set.singleton n
        eatBind _           = Set.empty


-- | Encode a context into a unique string.
--   This is a name for a particlar program context, which is guaranteed
--   to be from names of other contexts. This encoding can be used as 
--   a fresh name generator if you can base the names on the context they
--   are created in.
encodeCtx :: Ctx a n -> String
encodeCtx ctx0
 = go 1 ctx0
 where
  
  -- We indicate simulilar encosing contexts with by using an integer prefix
  -- for each component. We encode the position of particular alternatives
  -- and let-bindings with an integer suffix.
  go (n :: Int) ctx
   = let sn     = if n == 1
                        then "x" 
                        else "x" ++ show n
     in case ctx of
        CtxTop{}                        -> "Tt"
        
        CtxLAM       c@CtxLAM{} _ _     -> go (n + 1) c
        CtxLAM       c _ _              -> go 1 c ++ sn ++ "Lt"
        
        CtxLam       c@CtxLam{} _ _     -> go (n + 1) c
        CtxLam       c _ _              -> go 1 c ++ sn ++ "Lv"
        
        CtxAppLeft   c _ _              -> go 1 c ++ sn ++ "Al"
        CtxAppRight  c _ _              -> go 1 c ++ sn ++ "Ar"

        CtxLetBody   c@CtxLetBody{} _ _ -> go (n + 1) c
        CtxLetBody   c _ _              -> go 1 c ++ sn ++ "Eb"

        CtxLetLLet   c _ _ _            -> go 1 c ++ sn ++ "El"
        CtxLetLRec   c _ bxs  _ _ _     -> go 1 c ++ sn ++ "Er" ++ show (length bxs + 1)

        CtxCaseScrut c _ _              -> go 1 c ++ sn ++ "Cs"
        
        CtxCaseAlt   c _ _ alts _ _     -> go 1 c ++ sn ++ "Ca" ++ show (length alts + 1)
        
        CtxCastBody  c _ _              -> go 1 c ++ sn ++ "Sb"

