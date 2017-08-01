{-# LANGUAGE TypeFamilies, OverloadedStrings #-}
-- | A light simplification pass before conversion of desugared code to Core.
module DDC.Source.Tetra.Transform.Prep
        ( type S, evalState, newVar
        , desugarModule)
where
import DDC.Source.Tetra.Module
import DDC.Source.Tetra.Exp
import Data.Monoid
import Data.Text                                (Text)
import qualified Control.Monad.State.Strict     as S
import qualified Data.Text                      as Text


---------------------------------------------------------------------------------------------------
-- | State holding a variable name prefix and counter to
--   create fresh variable names.
type S          = S.State (Bool, Text, Int)
type Renames    = [(Name, Name)]


-- | Evaluate a desguaring computation,
--   using the given prefix for freshly introduced variables.
evalState :: Text -> S a -> a
evalState n c
 = S.evalState c (False, n, 0)


-- | Allocate a new named variable, yielding its associated bind and bound.
newVar :: Text -> S (Bind, Bound)
newVar pre
 = do   (p, n, i)   <- S.get
        let name = pre <> "$" <> n <> Text.pack (show i)
        S.put (p, n, i + 1)
        return  (BName name, UName name)


-- | Set the progress flag in the state.
progress :: S ()
progress
 = do   (_, n, i)       <- S.get
        S.put (True, n, i)


---------------------------------------------------------------------------------------------------
-- | Desguar a module.
--
--   We keep applying the prep transforms we have until they
--   stop making progress.
--
desugarModule :: Module Source -> S (Module Source)
desugarModule mm
 = do   (_, n, i) <- S.get
        S.put (False, n, i)

        mm'        <- desugarModule1 mm
        (p', _, _) <- S.get

        if p' then desugarModule mm'
              else return mm'


-- | Prepare a source module for conversion to core.
desugarModule1 :: Module Source -> S (Module Source)
desugarModule1 mm
 = do   ts'     <- mapM desugarTop $ moduleTops mm
        return  $ mm { moduleTops = ts' }


---------------------------------------------------------------------------------------------------
-- | Desugar a top-level definition.
desugarTop :: Top Source -> S (Top Source)
desugarTop tt
 = case tt of
        TopType{}       -> return tt
        TopData{}       -> return tt
        TopClause sp cl -> TopClause sp <$> desugarCl [] cl


---------------------------------------------------------------------------------------------------
-- | Desugar a clause.
desugarCl :: Renames -> Clause -> S Clause
desugarCl rns cl
 = case cl of
        SSig{}
         -> return cl

        SLet a b ps gxs
         -> do  ps'     <- mapM  desugarP ps
                gxs'    <- mapM (desugarGX rns) gxs
                return  $  SLet a b ps' gxs'


desugarP :: Param -> S Param
desugarP pp
 = case pp of
        MType{}         -> return pp
        MTerm w mt      -> MTerm     <$> desugarW w <*> return mt
        MImplicit w mt  -> MImplicit <$> desugarW w <*> return mt


---------------------------------------------------------------------------------------------------
-- | Desugar a guarded expression.
desugarGX :: Renames -> GuardedExp -> S GuardedExp
desugarGX rns gx
 = case gx of
        GGuard g gx'    -> GGuard <$> desugarG rns g <*> desugarGX rns gx'
        GExp   x        -> GExp   <$> desugarX rns x


---------------------------------------------------------------------------------------------------
-- | Desugar a guard.
desugarG :: Renames -> Guard -> S Guard
desugarG rns gg
 = case gg of
        GPat p x        -> GPat   <$> desugarW p <*> desugarX rns x
        GPred x         -> GPred  <$> desugarX rns x
        GDefault        -> return GDefault


---------------------------------------------------------------------------------------------------
-- | Desugar an expression.
desugarX :: Renames -> Exp -> S Exp
desugarX rns xx
 = case xx of
        -- Lift out nested box casts.
        --  This speculatively allocates the inner box,
        --  but means it's easier to find (run (box x)) pairs
        --
        --    let b1 = box (let b2 = box x3
        --                  in  x2)
        --    in x1
        --
        -- => let b2 = box x3 in
        --    let b1 = box x2 in
        --    x1
        --
        --    This transform makes b2 scope over x1 where it didn't before,
        --    so we rename it along the way to avoid variable clashes.
        --
        XLet (LLet b1
                  (XCast CastBox
                        (XLet  (LLet (XBindVarMT (BName n2) mt2)
                                     (XCast CastBox x3))
                                x2)))
                   x1
         -> do
                progress

                -- Make a new name for b2 and desugar x2 to force the rename.
                (b2', (UName n2')) <- newVar "x"
                x2'     <- desugarX ((n2, n2') : rns) x2

                desugarX rns
                 $  XLet (LLet (XBindVarMT b2' mt2) (XCast CastBox x3))
                 $  XLet (LLet b1                   (XCast CastBox x2'))
                 $  x1


        -- Eliminate trivial v1 = v2 bindings.
        XLet (LLet (XBindVarMT (BName n1) _) (XVar (UName n2))) x1
         -> do  let rns'    = (n1, n2) : rns
                progress
                desugarX rns' x1


        -- The match desugarer introduces case alternatives where the pattern
        -- is just a variable, which we can convert to a let-expression.
        XCase x0 ( AAltCase (PVar b) [GExp x1] : _)
         -> do  progress
                desugarX rns
                 $ XLet (LLet (XBindVarMT b Nothing) x0)
                 $ x1

        -- If the first pattern is a default and none of the other alternatives
        -- constrain the type of the scrutinee then the core type inferencer
        -- won't be able to determine the match type.
        XCase _x0 alts@(AAltCase PDefault [GExp x1] : _)
         | null [ p | AAltCase p@(PData _ _) _ <- alts]
         -> do  progress
                desugarX rns x1

        -- Translate out varible patterns.
        -- The core language does not include them, so we bind the
        -- scrutinee with a new name and substitute that for the
        -- name bound by the variable patterns.
        XCase x0 alts
         -- Only do the rewrite if at least one expression has
         -- such a variable pattern.
         |  ns    <- [n | AAltCase (PVar n) _ <- alts]
         ,  not $ null ns
         -> do
                progress

                -- Desugar the scrutinee.
                x0'     <- desugarX rns x0

                -- New variable to bind the scrutinee.
                (b, u@(UName nScrut)) <- newVar "xScrut"

                -- For each alternative, if it has a variable pattern
                -- then substitute the new name for it in the alternative.
                let desugarAlt (AAltCase (PVar (BName n1)) gxs)
                     = do let rns' =  (n1, nScrut) : rns
                          gxs'     <- mapM (desugarGX rns') gxs
                          return   $  AAltCase PDefault gxs'

                    desugarAlt (AAltCase p gxs)
                     = do gxs'     <- mapM (desugarGX rns) gxs
                          return   $  AAltCase p gxs'

                alts'   <- mapM desugarAlt alts

                -- The final expression.
                return
                 $ XLet  (LLet (XBindVarMT b Nothing) x0')
                 $ XCase (XVar u) alts'


        -- Eliminate (run (box x)) pairs.
        XCast CastBox (XCast CastRun x)
         -> do  progress
                desugarX rns x


        -- Lookup renames from the variable rename map.
        XVar (UName n0)
         -> let sink [] n               = n
                sink ((n1, n2) : rns') n
                 | n == n1              = sink rns' n2
                 | otherwise            = sink rns' n

            in do
                let n0' = sink rns n0
                if  n0 /= n0'
                 then do
                        progress
                        return $ XVar (UName n0')

                 else   return xx


        -- Convert XWhere to let expressions.
        XWhere _sp x cls
         -> do  x'      <- desugarX rns x
                cls'    <- mapM (desugarCl rns) cls
                return  $  XLet (LGroup True cls') x'


        -- Boilerplate.
        XAnnot a x              -> XAnnot a    <$> desugarX rns x
        XPrim{}                 -> return xx
        XFrag{}                 -> return xx
        XVar{}                  -> return xx
        XCon{}                  -> return xx
        XAbs  mb x              -> XAbs mb     <$> desugarX   rns x
        XApp  x1 x2             -> XApp        <$> desugarX   rns x1  <*> desugarArg rns x2
        XLet  lts x             -> XLet        <$> desugarLts rns lts <*> desugarX   rns x
        XCase x as              -> XCase       <$> desugarX   rns x   <*> mapM (desugarAC rns) as
        XCast c x               -> XCast c     <$> desugarX   rns x
        XDefix sp xs            -> XDefix sp   <$> mapM (desugarArg rns) xs
        XInfixOp{}              -> return xx
        XInfixVar{}             -> return xx
        XMatch   sp as x        -> XMatch   sp <$> mapM (desugarAM rns) as <*> desugarX rns x
        XAbsPat  sp ps p mt x   -> XAbsPat  sp ps p mt <$> desugarX rns x
        XLamCase sp alts        -> XLamCase sp <$> mapM (desugarAC rns) alts


---------------------------------------------------------------------------------------------------
-- | Desugar an argument.
desugarArg :: Renames -> Arg -> S Arg
desugarArg rns arg
 = case arg of
        RType{}         -> return arg
        RWitness{}      -> return arg
        RTerm x         -> RTerm     <$> desugarX   rns x
        RImplicit arg'  -> RImplicit <$> desugarArg rns arg'


---------------------------------------------------------------------------------------------------
-- | Desugar a case alternative.
desugarAC :: Renames -> AltCase -> S AltCase
desugarAC rns aa
 = case aa of
        AAltCase p gxs
         -> AAltCase <$> desugarW p <*> mapM (desugarGX rns) gxs


-- | Desugar a match alternative.
desugarAM :: Renames -> AltMatch -> S AltMatch
desugarAM rns (AAltMatch gx)
        = AAltMatch <$> desugarGX rns gx


-- | Desugar a pattern.
desugarW :: Pat -> S Pat
desugarW pp
 = case pp of
        -- Convert var binders where the variable is a wild card to
        -- the default pattern. We can't convert plain variable patterns
        -- to core.
        PVar BNone
         -> do  progress
                return PDefault

        PDefault        -> return PDefault
        PAt  b p        -> PAt b    <$> desugarW p
        PVar b          -> return $ PVar b
        PData dc ps     -> PData dc <$> mapM desugarW ps


---------------------------------------------------------------------------------------------------
-- | Desugar some let-bindings.
desugarLts :: Renames -> Lets -> S Lets
desugarLts rns lts
 = case lts of
        LLet mb x       -> LLet mb <$> desugarX rns x
        LPrivate{}      -> return lts

        LGroup bRec cls
         -> LGroup bRec <$> mapM (desugarCl rns) cls

        LRec bxs
         -> do  let (bs, xs)    =  unzip bxs
                xs'             <- mapM (desugarX rns) xs
                return          $ LRec $ zip bs xs'

