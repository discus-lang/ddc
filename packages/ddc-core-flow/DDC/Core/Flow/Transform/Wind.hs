
module DDC.Core.Flow.Transform.Wind
        ( RefInfo(..)
        , windModule)
where
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Core.Flow
import DDC.Core.Flow.Prim
import DDC.Core.Flow.Compounds
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
        = ContextLoop 
        { contextLoopName       :: Name
        , contextLoopAccs       :: [Bound Name] }

        | ContextGuard
        { _contextGuardCount     :: Name }
        deriving Show


-------------------------------------------------------------------------------
windModule :: Module () Name -> Module () Name
windModule m
 = let  body'   = windModuleBodyX (moduleBody m)
   in   m { moduleBody = body' }


-- | Do winding in the body of a module.
windModuleBodyX :: Exp () Name -> Exp () Name
windModuleBodyX xx
 = case xx of
        XLet a (LLet m b x1) x2
         -> let x1'     = windBodyX refMapZero [] x1
                x2'     = windModuleBodyX x2
            in  XLet a (LLet m b x1') x2'

        XLet a (LRec bxs) x2
         -> let bxs'    = [(b, windBodyX refMapZero [] x) | (b, x) <- bxs]
                x2'     = windModuleBodyX x2
            in  XLet a (LRec bxs') x2'

        XLet a lts x2
         -> let x2'     = windModuleBodyX x2
            in  XLet a lts x2'

        _ -> xx


-- | Do winding in the body of a function.
windBodyX 
        :: RefMap       -- ^ Info about how references are being rewritten.
        -> [Context]    -- ^ What loops and guards we're currently inside.
        -> Exp () Name  -- ^ Rewrite this expression.
        -> Exp () Name

windBodyX refMap context xx
 = let down = windBodyX refMap context
   in case xx of

        -- Detect ref allocation,
        --  and bind the initial value to a new variable.
        --
        --    ref     : Ref# type = new# [type] val
        -- => ref__0  : type      = val
        --
        XLet a (LLet LetStrict (BName nRef _) x) x2
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

            in  XLet a  (LLet LetStrict (BName nInit tElem) xVal)
                        (windBodyX refMap' context x2)


        -- Detect read,
        --  and rewrite to use the current version of the variable.
        --      val : type     = read# [type] ref
        --   => val : type     = ref_N
        --
        XLet a (LLet LetStrict bResult x) x2
         | Just ( NameOpStore OpStoreRead
                , [XType _tElem, XVar _ (UName nRef)] )   
                                        <- takeXPrimApps x
         , Just info    <- lookupRefInfo refMap nRef
         , Just nVal    <- nameOfRefInfo info
         ->     XLet a  (LLet LetStrict bResult (XVar a (UName nVal)))
                        (windBodyX refMap context x2)


        -- Detect loop combinator.
        XLet a (LLet LetStrict (BNone _) x) x2
         | Just ( NameOpLoop OpLoopLoopN
                , [ XType _tK, xLength
                  , XLam  _ bIx@(BName nIx _) xBody]) <- takeXPrimApps x
         -> let 
                -- Name of the new loop function.
                nLoop   = NameVar "loop"                             -- TODO: make a fresh name
                bLoop   = BName nLoop tLoop
                uLoop   = UName nLoop

                -- RefMap for before the loop, in the body, and after the loop.
                refMap_init  = refMap
                refMap_body  = bumpAllVersionsInRefMap refMap
                refMap_final = bumpAllVersionsInRefMap refMap_body

                -- Get binds and bounds for accumluators,
                --  to use in the body of the loop.
                bsAccs   = [ BName nVar (refInfoType info)
                                | info  <- refMapElems refMap_body
                                , let Just nVar    = nameOfRefInfo info ]

                usAccs  = takeSubstBoundsOfBinds bsAccs
                tsAccs  = map typeOfBind bsAccs


                -- The loop function itself will return us a tuple
                -- containing the final value of all the accumulators.
                tIndex  = typeOfBind bIx
                tResult = tTupleN tsAccs

                -- Type of the loop function.
                tLoop   = foldr tFunPE tResult (tIndex : tsAccs)


                -- Decend into loop body,
                --  and remember that we're doing the rewrite inside a loop context.
                context' = ContextLoop 
                                {  contextLoopName      = nLoop
                                ,  contextLoopAccs      = usAccs }
                         : context

                xBody'   = windBodyX refMap_body context' xBody


                -- Create the loop driver.
                --  This is the code that tests for the end-of-loop condition.
                xDriver = xLams a (bIx : bsAccs) 
                        $ XCase a (XVar a (UName nIx)) 
                                [ AAlt (PData (dcNat 0) []) xResult
                                , AAlt PDefault xBody' ]

                xResult = xApps a (XCon a (dcTupleN $ length tsAccs)) 
                                  (  [XType t  | t <- tsAccs]
                                  ++ [XVar a u | u <- usAccs] )


                -- Initial values of index and accumulators.
                xsInit  = xLength
                        : [ XVar a (UName nVar)
                                | info  <- refMapElems refMap_init
                                , let Just nVar = nameOfRefInfo info ]


                -- Decend into loop postlude.
                bsFinal = [ BName nVar (refInfoType info)
                                | info  <- refMapElems refMap_final
                                , let Just nVar = nameOfRefInfo info ]

                x2'     = windBodyX refMap_final context x2


            in  XLet  a  (LRec [(bLoop, xDriver)]) 
             $  XCase a (xApps a (XVar a uLoop) xsInit)
                        [ AAlt (PData (dcTupleN $ length tsAccs) bsFinal) x2' ]


        -- Detect guard combinator.
        XLet a (LLet LetStrict (BNone _) x) x2
         | Just ( NameOpLoop OpLoopGuard
                , [ XVar _ (UName nCountRef)
                  , xFlag
                  , XLam _ bCount xBody ])       <- takeXPrimApps x
         -> let 
                Just infoCount  = lookupRefInfo refMap nCountRef

                Just nCount     = nameOfRefInfo infoCount

                xBody'  = XLet a (LLet LetStrict bCount (XVar a (UName nCount)))
                        $ down xBody

            in  XCase a xFlag 
                        [ AAlt (PData (dcBool True) []) xBody'
                        , AAlt PDefault (down x2) ]


        -- Boilerplate -----------------------------------------
        XVar{}          -> xx
        XCon{}          -> xx
        XLAM a b x      -> XLAM a b (down x)
        XLam a b x      -> XLam a b (down x)

        XApp{}          -> xx

        XLet a (LLet m b x) x2
         -> XLet a (LLet m b (down x)) 
                   (down x2)

        XLet a (LRec bxs) x2
         -> XLet a (LRec [(b, down x) | (b, x) <- bxs])
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

