
module DDC.Core.Flow.Transform.Wind
        ( RefInfo(..)
        , windModule)
where
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Core.Flow
import DDC.Core.Flow.Prim
import DDC.Core.Compounds
import qualified Data.Map       as Map
import Data.Map                 (Map)


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


nameOfRefInfo :: RefInfo -> Maybe Name
nameOfRefInfo info
 = case refInfoName info of
        NameVar str     
          -> Just $ NameVar (str ++ "__" ++ show (refInfoVersionNumber info))
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
windModule :: Module () Name -> Module () Name
windModule m
 = let  body'   = windModuleBodyX (moduleBody m)
   in   m { moduleBody = body' }


-- | Do winding in the body of a module.
windModuleBodyX :: Exp () Name -> Exp () Name
windModuleBodyX xx
 = case xx of
        XLet a (LLet m b x1) x2
         -> let x1'     = windBodyX refMapZero x1
                x2'     = windModuleBodyX x2
            in  XLet a (LLet m b x1') x2'

        XLet a (LRec bxs) x2
         -> let bxs'    = [(b, windBodyX refMapZero x) | (b, x) <- bxs]
                x2'     = windModuleBodyX x2
            in  XLet a (LRec bxs') x2'

        XLet a lts x2
         -> let x2'     = windModuleBodyX x2
            in  XLet a lts x2'

        _ -> xx


-- | Do winding in the body of a function.
windBodyX :: RefMap -> Exp () Name -> Exp () Name
windBodyX refMap xx
 = let down = windBodyX refMap 
   in case xx of

        -- Detect ref allocation,
        --  and bind the initial value to a new variable.
        --
        --    ref        : Ref# type = new# [type] val
        -- => ref__init  : type      = val
        --
        XLet a (LLet LetStrict (BName nRef _) x) x2
         | Just ( NameOpStore OpStoreNew
                , [XType tElem, xVal] )    <- takeXPrimApps x
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
                        (windBodyX refMap' x2)


        -- Detect loop combinator.
        XLet a (LLet LetStrict (BNone _) x) x2
         | Just ( NameOpLoop OpLoopLoopN
                , [ XType _tK, _xLength
                  , XLam  _ bIx xBody]) <- takeXPrimApps x
         -> let 
                nLoop   = NameVar "loop"                        -- TODO: make a fresh name
                bLoop   = BName nLoop tUnit                     -- TODO: update to return type


                -- Decend into body of loop.
                refMap1 = bumpAllVersionsInRefMap refMap
                bsRef   = [ BName nVar (refInfoType info)
                                | info  <- refMapElems refMap1
                                , let Just nVar    = nameOfRefInfo info ]

                xBody'  = XLam a bIx (xLams a bsRef xBody)


                -- Decend into loop postlude.
                refMap2 = bumpAllVersionsInRefMap refMap1
                x2'     = windBodyX refMap2 x2


            in  XLet a  (LRec [(bLoop, xBody')]) 
                        x2'


        -- Boilerplate -----------------------------------------
        XVar{}          -> xx
        XCon{}          -> xx
        XLAM a b x      -> XLAM a b (down x)
        XLam a b x      -> XLAM a b (down x)

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
         -> let  x'      = windBodyX refMap x
            in  XCast a c x'

        XType{}         -> xx
        XWitness{}      -> xx

