
-- | Expliciation is the of converting all implicit paramters and arguments
--   to explicit ones, and also substituting in type equations. We do this
--   as a prep stage before converting a module to a lower level fragment
--   like Core Salt.
--
--   When converting implicit arguments to explicit ones we do not actually
--   search the context for an appropriate binder, we just convert the form
--   of the parameters and arguments to explicit ones.
--
module DDC.Core.Transform.Expliciate
        ( expliciateModule
        , expliciateType
        , expliciateExp)
where
import DDC.Type.Exp.Simple
import DDC.Type.DataDef
import DDC.Core.Module
import DDC.Core.Exp
import Data.Map                 (Map)
import qualified DDC.Type.Sum   as Sum
import qualified Data.Map       as Map


---------------------------------------------------------------------------------------------------
-- | Expliciate a module
expliciateModule
        :: Ord n
        => Module a n -> Module a n

expliciateModule mm
 = let eqns    = Map.fromList
                $ [(n, t) | (n, (_k, t)) <- moduleTypeDefs mm]

       downT   = expliciateType eqns

   in  mm
        { moduleExportValues    = [ (n, mapTypeOfExportValue downT ex)
                                  | (n, ex) <- moduleExportValues mm ]

        , moduleImportCaps      = [ (n, mapTypeOfImportCap    downT im)
                                  | (n, im) <- moduleImportCaps   mm ]

        , moduleImportValues    = [ (n, mapTypeOfImportValue  downT im)
                                  | (n, im) <- moduleImportValues mm ]

        , moduleImportDataDefs  = [ (n, mapTypeOfDataDef downT def)
                                  | (n, def) <- moduleImportDataDefs mm ]

          -- Type defs are zapped because we're inlining them all anyway,
          -- and they can't be recursive.
        , moduleImportTypeDefs  = []

        , moduleLocalDataDefs   = [ (n, mapTypeOfDataDef downT def)
                                  | (n, def) <- moduleLocalDataDefs mm ]

          -- Type defs are zapped because we're inlining them all anyway,
          -- and they can't be recursive.
        , moduleLocalTypeDefs   = []

        , moduleBody            = expliciateExp eqns (moduleBody mm)
        }


---------------------------------------------------------------------------------------------------
-- | Make type explicit by substituting in the definitions for all type synonyms.
expliciateType :: Ord n => Map n (Type n) -> Type n -> Type n
expliciateType eqns tt
 = let down = expliciateType eqns
   in case tt of
        TCon tc
         -> case tc of
                TyConBound n
                  -> case Map.lookup n eqns of
                        Nothing -> tt
                        Just t  -> expliciateType eqns t

                TyConSpec TcConFunImplicit
                  -> TCon $ TyConSpec $ TcConFunExplicit

                _ -> tt

        TVar    u       -> TVar u
        TAbs    b tBody -> TAbs b $ down tBody
        TApp    t1 t2   -> TApp (down t1) (down t2)
        TForall b tBody -> TForall b (down tBody)
        TSum    ts      -> TSum $ Sum.fromList (Sum.kindOfSum ts) $ map down $ Sum.toList ts
        TRow    r       -> TRow [ (l, down t) | (l, t) <- r ]


expliciateBind
        :: Ord n
        => Map n (Type n)
        -> Bind n -> Bind n

expliciateBind eqns bb
        = replaceTypeOfBind (expliciateType eqns (typeOfBind bb)) bb


---------------------------------------------------------------------------------------------------
-- | Expliciate an expression.
expliciateExp :: Ord n => Map n (Type n) -> Exp a n -> Exp a n
expliciateExp eqns xx
 = case xx of
        XVar{}          -> xx
        XAtom{}         -> xx

        XAbs a m xBody  -> XAbs  a (expliciateParam eqns m)
                                   (expliciateExp   eqns xBody)

        XApp a x1 a2    -> XApp  a (expliciateExp   eqns x1)
                                   (expliciateArg   eqns a2)

        XLet a lts x2   -> XLet  a (expliciateLets  eqns lts)
                                   (expliciateExp   eqns x2)

        XCase a x alts  -> XCase a (expliciateExp   eqns x)
                                   (map (expliciateAlt eqns) alts)

        XCast a c x     -> XCast a (expliciateCast  eqns c)
                                   (expliciateExp   eqns x)


-- | Expliciate a parameter.
expliciateParam
        :: Ord n
        => Map n (Type n)
        -> Param n -> Param n

expliciateParam eqns pp
 = case pp of
        MType b         -> MType (expliciateBind eqns b)
        MTerm b         -> MTerm (expliciateBind eqns b)
        MImplicit b     -> MTerm (expliciateBind eqns b)


-- | Expliciate an argument.
expliciateArg
        :: Ord n
        => Map n (Type n)
        -> Arg a n -> Arg a n

expliciateArg eqns arg
 = case arg of
        RType t         -> RType $ expliciateType eqns t
        RTerm x         -> RTerm $ expliciateExp  eqns x
        RWitness{}      -> arg
        RImplicit arg'  -> expliciateArg  eqns arg'


-- | Expliciate some let bindings.
expliciateLets
        :: Ord n
        => Map n (Type n)
        -> Lets a n -> Lets a n

expliciateLets eqns lts
 = case lts of
        LLet b x
         -> LLet (expliciateBind  eqns b) (expliciateExp eqns x)

        LRec bxs
         -> LRec [(expliciateBind eqns b, expliciateExp eqns x)
                 | (b, x) <- bxs]

        LPrivate bs mts bsWit
         -> LPrivate
                (map  (expliciateBind eqns) bs)
                (fmap (expliciateType eqns) mts)
                (map  (expliciateBind eqns) bsWit)


-- | Expliciate a case alternative.
expliciateAlt
        :: Ord n
        => Map n (Type n)
        -> Alt a n -> Alt a n

expliciateAlt eqns aa
 = case aa of
        AAlt w x        -> AAlt (expliciatePat eqns w) (expliciateExp eqns x)


-- | Expliciate a pattern.
expliciatePat
        :: Ord n
        => Map n (Type n)
        -> Pat n -> Pat n

expliciatePat eqns ww
 = case ww of
        PDefault        -> ww
        PData dc bs     -> PData dc (map (expliciateBind eqns) bs)


-- | Expliciate a cast.
expliciateCast
        :: Ord n
        => Map n (Type n)
        -> Cast a n -> Cast a n

expliciateCast eqns cc
 = case cc of
        CastWeakenEffect eff
         -> CastWeakenEffect (expliciateType eqns eff)

        CastPurify{}    -> cc
        CastBox{}       -> cc
        CastRun{}       -> cc

