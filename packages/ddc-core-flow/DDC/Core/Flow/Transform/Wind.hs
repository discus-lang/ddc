
-- | Convert a loop expressed with the loopn# and guard# combinators into
--   a tail recursive loop with accumulators.
--
--   ASUMPTIONS:
--   * No nested loops.
--      We could support these, but we don't yet.
--  
--   * Outer control flow is only defined via the loopn# and guard# 
--     combinators.
--
--   * References don't escape, 
--      so they're not stored in data structures or captured in closures.
--
--   * No aliasing of references, 
--      so updating ref with a particular name does not affect any other ref.
-- 
--   * Refs holding loop counters for loopn# and entry counters for guard# 
--     are not written to by any other statements.
-- 
--   The above assumptions are true for code generated with the lowering
--   transform, but won't be true for general code, and we don't check for
--   violiations of these assumptions.
--
module DDC.Core.Flow.Transform.Wind
        ( RefInfo(..)
        , windModule)
where
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Core.Flow
import DDC.Core.Flow.Prim
import DDC.Core.Compounds
import DDC.Core.Flow.Compounds  (tNat, dcNat, dcTupleN, dcBool, tTupleN)
import qualified Data.Map       as Map
import Data.Map                 (Map)


-------------------------------------------------------------------------------
-- | Current information for a reference.
data RefInfo
        = RefInfo
        { refInfoName           :: Name
        , refInfoType           :: Type Name
        , refInfoCurrent        :: Name 
        , refInfoVersionNumber  :: Int }

data RefMap
        = RefMap (Map Name RefInfo)

refMapZero :: RefMap
refMapZero = RefMap Map.empty

refMapElems :: RefMap -> [RefInfo]
refMapElems (RefMap mm)
        = Map.elems mm


-- | Insert a new `RefInfo` record into the map.
insertRefInfo  :: RefInfo -> RefMap -> RefMap
insertRefInfo info (RefMap mm)
 = RefMap (Map.insert (refInfoName info) info mm)


-- | Lookup a `RefInfo` record from the map.
lookupRefInfo  :: RefMap -> Name -> Maybe RefInfo
lookupRefInfo (RefMap mm) n
 = Map.lookup n mm


-- | Get the name of the current version of a value from a `RefInfo`.
nameOfRefInfo :: RefInfo -> Maybe Name
nameOfRefInfo info
 = case refInfoName info of
        NameVar str     
          -> Just $ NameVar (str ++ "_" ++ show (refInfoVersionNumber info))
        _ -> Nothing


-- | Bump the version number of a `RefInfo`
bumpVersionOfRefInfo :: RefInfo -> RefInfo
bumpVersionOfRefInfo info
 = info { refInfoVersionNumber = refInfoVersionNumber info + 1 }


-- | Bump the version number of one element of a `RefMap`.
bumpVersionInRefMap  :: Name -> RefMap -> RefMap
bumpVersionInRefMap n (RefMap mm)
 = RefMap $ Map.update (Just . bumpVersionOfRefInfo) n mm


-- | Bump the version numbers of all elements of a `RefMap`.
bumpAllVersionsInRefMap :: RefMap -> RefMap
bumpAllVersionsInRefMap mm
 = foldr bumpVersionInRefMap mm $ map refInfoName $ refMapElems mm


-------------------------------------------------------------------------------
data Context
        -- | We're currently in the body of a loop.
        = ContextLoop 
        { contextLoopName       :: Name
        , contextLoopCounter    :: Name
        , contextLoopAccs       :: [Name] }

        -- | We're currently in the body of a guard.
        | ContextGuard
        { -- | Name of the entry counter,
          --   the number of times this guard has matched.
          contextGuardCounter   :: Name

          -- | Whether we're in the matching or non-matching branch.
        , contextGuardFlag      :: Bool }
        deriving Show


-- | Build a tailcall from the current context.
--   This tells us where to go after finishing the body of a loop.
makeTailCallFromContexts :: a -> RefMap -> [Context] -> Exp a Name
makeTailCallFromContexts a refMap context@(ContextLoop nLoop _ _ : _)
 = let  
        xLoop   = XVar a (UName nLoop)
        xArgs   = slurpArgUpdates a refMap [] context

   in   xApps a xLoop xArgs
   
makeTailCallFromContexts _ _ _
 = error "makeTailCallFromContexts: sorry"


-- | Slurp expressions to update each of the accumulators of the loop.
--   TODO: Don't assume there have been no other updates to the loop counter
--         and guard entry refs.
slurpArgUpdates 
        :: a
        -> RefMap
        -> [(Name, Exp a Name)] 
        -> [Context] 
        -> [Exp a Name]

slurpArgUpdates a refMap [] (ContextLoop _ nCounter nAccs : more)
 = let
        -- Expression to update loop counter.
        nxCounter' 
         = ( nCounter
           , xIncrement a (XVar a (UName nCounter)) )

        -- Updated accumulators.
        nxAccs'    
         = [ (nAcc, XVar a (UName nAcc'))
                | nAcc          <- nAccs
                , let Just info  = lookupRefInfo refMap nAcc
                , let Just nAcc' = nameOfRefInfo info ]

   in   slurpArgUpdates a refMap (nxCounter' : nxAccs') more

-- If we're inside the true branch of a guard then update
-- the associated entry counter for the guard.
slurpArgUpdates a refMap args (ContextGuard nCounter flag : more)
 | flag == True
 = let  
        update []               = []
        update ((n, x) : args')
         | n == nCounter        = (n, xIncrement a x) : update args'
         | otherwise            = (n, x)              : update args'

   in   slurpArgUpdates a refMap (update args) more

 | otherwise
 =      slurpArgUpdates a refMap args more

slurpArgUpdates _ _ _   (ContextLoop{} : _)
 = error "slurpArgUpdates: nested loops not supported"



slurpArgUpdates _ _ args []
 = map snd args


-- | Build an expression that increments a natural.
xIncrement :: a -> Exp a Name -> Exp a Name
xIncrement a xx
        = xApps a (XVar a (UPrim (NamePrimArith PrimArithAdd) 
                                 (typePrimArith PrimArithAdd)))
                  [ XType tNat, xx, XCon a (dcNat 1) ]

-- | Build an expression that substracts two integers.
xSubInt    :: a -> Exp a Name -> Exp a Name -> Exp a Name
xSubInt a x1 x2
        = xApps a (XVar a (UPrim (NamePrimArith PrimArithSub)
                                 (typePrimArith PrimArithSub)))
                  [ XType tNat, x1, x2]


-------------------------------------------------------------------------------
windModule :: Module () Name -> Module () Name
windModule m
 = let  body'   = windModuleBodyX (moduleBody m)
   in   m { moduleBody = body' }


-- | Do winding in the body of a module.
windModuleBodyX :: Exp () Name -> Exp () Name
windModuleBodyX xx
 = case xx of
        XLet a (LLet b x1) x2
         -> let x1'     = windBodyX refMapZero [] x1
                x2'     = windModuleBodyX x2
            in  XLet a (LLet b x1') x2'

        XLet a (LRec bxs) x2
         -> let bxs'    = [(b, windBodyX refMapZero [] x) | (b, x) <- bxs]
                x2'     = windModuleBodyX x2
            in  XLet a (LRec bxs') x2'

        XLet a lts x2
         -> let x2'     = windModuleBodyX x2
            in  XLet a lts x2'

        _ -> xx


-------------------------------------------------------------------------------
-- | Do winding in the body of a function.
windBodyX 
        :: RefMap       -- ^ Info about how references are being rewritten.
        -> [Context]    -- ^ What loops and guards we're currently inside.
        -> Exp () Name  -- ^ Rewrite this expression.
        -> Exp () Name

windBodyX refMap context xx
 = let down = windBodyX refMap context
   in case xx of

        -----------------------------------------
        -- Detect ref allocation,
        --  to bind the initial value to a new variable.
        --
        --    ref     : Ref# type = new# [type] val
        -- => ref__0  : type      = val
        --
        XLet a (LLet (BName nRef _) x) x2
         | Just ( NameOpStore OpStoreNew
                , [XType tElem, xVal] ) <- takeXPrimApps x
         -> let 
                -- Add the new ref record to the map.
                info        = RefInfo 
                            { refInfoName          = nRef
                            , refInfoType          = tElem
                            , refInfoCurrent       = nInit 
                            , refInfoVersionNumber = 0 }

                -- Rewrite the statement that creates a new ref to one
                -- that just binds the initial value.
                Just nInit  = nameOfRefInfo info
                refMap'     = insertRefInfo info refMap

            in  XLet a  (LLet (BName nInit tElem) xVal)
                        (windBodyX refMap' context x2)


        -----------------------------------------
        -- Detect ref read,
        --  and rewrite to use the current version of the variable.
        --      val : type     = read# [type] ref
        --   => val : type     = ref_N
        --
        XLet a (LLet bResult x) x2
         | Just ( NameOpStore OpStoreRead
                , [XType _tElem, XVar _ (UName nRef)] )   
                                        <- takeXPrimApps x
         , Just info    <- lookupRefInfo refMap nRef
         , Just nVal    <- nameOfRefInfo info
         ->     XLet a  (LLet bResult (XVar a (UName nVal)))
                        (windBodyX refMap context x2)


        -----------------------------------------
        -- Detect ref write,
        --  to just bind the new value.
        XLet a (LLet (BNone _) x) x2
         | Just ( NameOpStore OpStoreWrite 
                , [XType _tElem, XVar _ (UName nRef), xVal])
                                        <- takeXPrimApps x
         , refMap'      <- bumpVersionInRefMap nRef refMap
         , Just info    <- lookupRefInfo refMap' nRef
         , Just nVal    <- nameOfRefInfo info
         , tVal         <- refInfoType info
         ->     XLet a  (LLet (BName nVal tVal) xVal)
                        (windBodyX refMap' context x2)


        -----------------------------------------
        -- Detect loop combinator.
        XLet a (LLet (BNone _) x) x2
         | Just ( NameOpLoop OpLoopLoopN
                , [ XType tK, xLength
                  , XLam  _ bIx@(BName nIx _) xBody]) <- takeXPrimApps x
         -> let 
                -- Name of the new loop function.
                TVar (UName (NameVar strK))    = tK
                nLoop   = NameVar ("loop_" ++ strK)     -- TODO: make a fresh name
                bLoop   = BName nLoop tLoop
                uLoop   = UName nLoop

                nLength = NameVar (strK ++ "_length")   -- TODO: make a fresh name
                bLength = BName nLength tNat
                uLength = UName nLength

                -- RefMap for before the loop, in the body, and after the loop.
                refMap_init  = refMap
                refMap_body  = bumpAllVersionsInRefMap refMap
                refMap_final = bumpAllVersionsInRefMap refMap_body

                -- Get binds and bounds for accumluators,
                --  to use in the body of the loop.
                bsAccs   = [ BName nVar (refInfoType info)
                                | info  <- refMapElems refMap_body
                                , let Just nVar    = nameOfRefInfo info ]

                usAccs          = takeSubstBoundsOfBinds bsAccs
                tsAccs          = map typeOfBind bsAccs


                -- The loop function itself will return us a tuple
                -- containing the final value of all the accumulators.
                tIndex  = typeOfBind bIx
                tResult = loopResultT tsAccs

                -- Type of the loop function.
                tLoop   = foldr tFunPE tResult (tIndex : tsAccs)


                -- Decend into loop body,
                --  and remember that we're doing the rewrite inside a loop context.
                context' =  context
                         ++ [ ContextLoop 
                                { contextLoopName      = nLoop
                                , contextLoopCounter   = nIx
                                , contextLoopAccs      = map refInfoName 
                                                       $ refMapElems refMap_body } ]

                xBody'   = windBodyX refMap_body context' xBody


                -- Create the loop driver.
                --  This is the code that tests for the end-of-loop condition.
                xDriver = xLams a (bIx : bsAccs) 
                        $ XCase a (xSubInt a (XVar a uLength) (XVar a (UName nIx)))
                                [ AAlt (PData (dcNat 0) []) xResult
                                , AAlt PDefault xBody' ]

                xResult = loopResultX a 
                                tsAccs
                                [XVar a u | u <- usAccs]

                -- Initial values of index and accumulators.
                xsInit  = XCon a (dcNat 0)
                        : [ XVar a (UName nVar)
                                | info  <- refMapElems refMap_init
                                , let Just nVar = nameOfRefInfo info ]


                -- Decend into loop postlude.
                bsFinal = [ BName nVar (refInfoType info)
                                | info  <- refMapElems refMap_final
                                , let Just nVar = nameOfRefInfo info ]

                x2'     = windBodyX refMap_final context x2


            in  XLet  a  (LLet bLength (xNatOfRateNat tK xLength))
              $ XLet  a  (LRec [(bLoop, xDriver)]) 
              $ runUnpackLoop 
                        a 
                        tsAccs                          -- Types of accumulators.
                        (xApps a (XVar a uLoop) xsInit) -- Expression to invoke loop
                        bsFinal                         -- Binders for final accumulators
                        x2'                             -- Continuation expression


        -----------------------------------------
        -- Detect guard combinator.
        XLet a (LLet (BNone _) x) x2
         | Just ( NameOpLoop OpLoopGuard
                , [ XVar _ (UName nCountRef)
                  , xFlag
                  , XLam _ bCount xBody ])       <- takeXPrimApps x
         -> let 
                Just infoCount  = lookupRefInfo refMap nCountRef

                Just nCount     = nameOfRefInfo infoCount

                context' = context
                         ++ [ ContextGuard
                                { contextGuardCounter = nCountRef
                                , contextGuardFlag    = True }  ]

                xBody'  = XLet a (LLet bCount (XVar a (UName nCount)))
                        $ windBodyX refMap context' xBody

            in  XCase a xFlag 
                        [ AAlt (PData (dcBool True) []) xBody'
                        , AAlt PDefault (down x2) ]


        -----------------------------------------
        -- Detect end value.
        --   When we hit a Unit at the top level of the body of a loop then
        --   we know it's time to do the recursive call.
        XCon a dc
         | dc == dcUnit
         -> makeTailCallFromContexts a refMap context


        -- Boilerplate --------------------------
        XVar{}          -> xx
        XCon{}          -> xx
        XLAM a b x      -> XLAM a b (down x)
        XLam a b x      -> XLam a b (down x)

        XApp{}          -> xx

        -- Decend into nest let binding.
        --  We need to drop the contexts because we never do a tail-call
        --  from a nested binding.
        XLet a (LLet b x) x2
         -> XLet a (LLet b (windBodyX refMap [] x)) 
                   (down x2)

        XLet a (LRec bxs) x2
         -> XLet a (LRec [(b, windBodyX refMap [] x) | (b, x) <- bxs])
                   (down x2)

        XLet a lts x2
         -> XLet a lts (down x2)

        XCase{}
         -> error "windBodyX: not finished"

        XCast a c x
         -> let  x'      = windBodyX refMap context x
            in  XCast a c x'

        XType{}         -> xx
        XWitness{}      -> xx


xNatOfRateNat :: Type Name -> Exp () Name -> Exp () Name
xNatOfRateNat tK xR
        = xApps () 
                (xVarOpFlow OpFlowNatOfRateNat)
                [XType tK, xR]

xVarOpFlow :: OpFlow -> Exp () Name
xVarOpFlow op
        = XVar  () (UPrim (NameOpFlow op) (typeOpFlow op))


-------------------------------------------------------------------------------
-- | Make the type of a loop result, 
--   given the types of the accumulators for that loop. 
--
--   If we have no accumulators, return Unit.
--   If we have just one, return that value.
--   If more, then package them into a tuple.
--
loopResultT :: [Type Name] -> Type Name
loopResultT tsAccs
 = case tsAccs of
        []      -> tUnit
        [tAcc]  -> tAcc
        _       -> tTupleN tsAccs


-- | Make a loop result,
--   given the expressions for the accumulators.
loopResultX :: a -> [Type Name] -> [Exp a Name] -> Exp a Name
loopResultX a tsAccs xsAccs
 = case xsAccs of
        []      -> xUnit a
        [x]     -> x
        _       -> xApps a (XCon a (dcTupleN $ length tsAccs)) 
                           ([XType t  | t <- tsAccs] ++ xsAccs)


-- | Call a loop, and unpack its result.
runUnpackLoop 
        :: a 
        -> [Type Name]  -- ^ Types of accumulators.
        -> Exp a Name   -- ^ Expression to invoke the loop.
        -> [Bind Name]  -- ^ Binders for the accumulated values.
        -> Exp a Name   -- ^ Continuation expression.
        -> Exp a Name

runUnpackLoop a tsAccs xRunLoop bsAcc xCont
 | []   <- tsAccs
 =      XLet a (LLet (BNone tUnit) xRunLoop) xCont

 | [_t]  <- tsAccs
 , [b]   <- bsAcc
 =      XLet a (LLet b xRunLoop) xCont

 | otherwise
 =      XCase a xRunLoop
                [ AAlt (PData (dcTupleN $ length tsAccs) bsAcc) xCont ]




