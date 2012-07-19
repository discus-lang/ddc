
-- | Conversion of Disciple Lite to Disciple Salt.
--
module DDC.Core.Lite.Convert
        ( toSalt
        , Error(..))
where
import DDC.Core.Lite.Convert.Data
import DDC.Core.Lite.Convert.Type
import DDC.Core.Lite.Convert.Base
import DDC.Core.Salt.Platform
import DDC.Core.Module
import DDC.Core.Compounds
import DDC.Core.Predicates
import DDC.Core.Exp
import DDC.Type.Compounds
import DDC.Type.Predicates
import DDC.Type.Universe
import DDC.Type.DataDef
import DDC.Type.Check.Monad              (throw, result)
import DDC.Core.Check                    (AnTEC(..))
import qualified DDC.Core.Lite.Name      as L
import qualified DDC.Core.Salt.Runtime   as O
import qualified DDC.Core.Salt.Name      as O
import qualified DDC.Core.Salt.Env       as O
import qualified Data.Map                as Map
import Control.Monad


-- | Convert a Disciple Core Lite module to Disciple Core Salt.
--
--   Case expressions on alrebraic data values are converted into ones that just
--   check the tag, while data constructors are unfolded into explicit allocation
--   and field initialization primops. 
--
--   TODO: Add the alternatives that force and follow lazy thunks and indirections.
--   TODO: Expand partial and over-applications into code that explicitly builds
--         and applies thunks.
--
--   The input module needs to be:
--      well typed,
--      fully named with no deBruijn indices,
--      have all functions defined at top-level,
--      have type annotations on every bound variable and constructor
--      a-normalised
--      If not then `Error`.
--
--   The output code contains:
--      debruijn indices.
--       these which need to be eliminated before it will pass the Salt fragment checks.
--
toSalt
        :: Show a
        => Platform
        -> DataDefs L.Name
        -> Module (AnTEC a L.Name) L.Name 
        -> Either (Error a) (Module a O.Name)
toSalt pp defs mm
 = result $ convertM pp defs mm



-- Module ---------------------------------------------------------------------
convertM 
        :: Show a
        => Platform
        -> DataDefs L.Name 
        -> Module (AnTEC a L.Name) L.Name 
        -> ConvertM a (Module a O.Name)

convertM pp defsPrim mm
  = do  let defs = defsPrim
        x'              <- convertBodyX pp defs $ moduleBody mm
        tsImports'      <- liftM Map.fromList
                        $  mapM convertImportM  
                        $  Map.toList
                        $  moduleImportTypes mm

        return $ ModuleCore
         { moduleName           = moduleName mm
         , moduleExportKinds    = Map.empty
         , moduleExportTypes    = Map.empty
         , moduleImportKinds    = Map.empty
         , moduleImportTypes    = Map.union O.runtimeImportSigs tsImports'
         , moduleBody           = x' }


-- | Convert an import spec.
convertImportM
        :: (L.Name, (QualName L.Name, Type L.Name))
        -> ConvertM a (O.Name, (QualName O.Name, Type O.Name))

convertImportM (n, (qn, t))
 = do   n'      <- convertBindNameM n
        qn'     <- convertQualNameM qn
        t'      <- convertT t
        return  (n', (qn', t'))


-- | Convert a qualified name.
convertQualNameM
        :: QualName L.Name 
        -> ConvertM a (QualName O.Name)

convertQualNameM (QualName mn n)
 = do   n'      <- convertBindNameM n
        return  $ QualName mn n'


-- Exp -------------------------------------------------------------------------
convertBodyX 
        :: Show a 
        => Platform
        -> DataDefs L.Name 
        -> Exp (AnTEC a L.Name) L.Name 
        -> ConvertM a (Exp a O.Name)

convertBodyX pp defs xx
 = case xx of
        -- TODO: check there are no debruijn indices.

        XVar a u
         -> do  let a'  = annotTail a
                u'      <- convertU u
                return  $  XVar a' u'

        XCon a u
         -> do  let a'  = annotTail a
                (xx', _) <- convertC defs a' u
                return  xx'

        -- Strip out non-region lambdas.
        XLAM a b x
         | isRegionKind $ typeOfBind b
         -> do  let a'  = annotTail a
                b'      <- convertB b
                x'      <- convertBodyX pp defs x
                return $ XLAM a' b' x'

         | otherwise
         -> convertBodyX pp defs x


        -- Keep value binders but ditch witness binders for now.
        XLam a b x
         -> case universeFromType1 (typeOfBind b) of
             Just UniverseData    
              -> liftM3 XLam 
                        (return $ annotTail a) 
                        (convertB b) 
                        (convertBodyX pp defs x)

             Just UniverseWitness 
              -> convertBodyX pp defs x

             _          -> error "toSaltX: unexpected binder" -- throw ErrorMistyped

        XApp{}
         -> do  x'      <- convertArgX pp defs xx
                return  $ x'

        XLet a (LRec bxs) x2
         -> do  let (bs, xs)    = unzip bxs
                bs'     <- mapM convertB bs
                xs'     <- mapM (convertBodyX pp defs) xs
                x2'     <- convertBodyX pp defs x2
                return $ XLet (annotTail a) (LRec $ zip bs' xs') x2'

        XLet a (LLet LetStrict b x1) x2
         -> do  b'      <- convertB b
                x1'     <- convertArgX  pp defs x1
                x2'     <- convertBodyX pp defs x2
                return  $ XLet (annotTail a) (LLet LetStrict b' x1') x2'

        XLet _ (LLet LetLazy{} _ _) _
         -> error "toSaltX: XLet lazy not handled"

        XLet _ (LLetRegion _ _) x2
         -> do  convertBodyX pp defs x2

        XLet _ LWithRegion{} _
         -> throw ErrorMalformed


        -- Match against literal unboxed values.
        --  The branch is against the literal value itself.
        --  TODO: We can't branch against float literals.
        --        Matches against float literals should be desugared into if-then-else chains.
        --        Same for string literals.
        XCase (AnTEC _t _ _ a') xScrut@(XVar _ uScrut) alts
         | TCon (TyConBound (UPrim nType _))    <- typeOfBound uScrut
         , L.NamePrimTyCon _                    <- nType
         -> do  xScrut' <- convertArgX pp defs xScrut
                alts'   <- mapM (convertA pp defs a' uScrut) alts
                return  $  XCase a' xScrut' alts'

        -- Match against finite algebraic data.
        --   The branch is against the constructor tag.
        XCase (AnTEC t _ _ a') x@(XVar _ uX) alts  
         -> do  x'@(XVar _ uX') <- convertArgX pp defs x
                t'              <- convertT t
                alts'           <- mapM (convertA pp defs a' uX) alts

                let asDefault
                        | any isPDefault [p | AAlt p _ <- alts]   
                        = []

                        | otherwise     
                        = [AAlt PDefault (O.xFail a' t')]

                let Just tPrime = takePrimeRegion (typeOfBound uX')
                return  $ XCase a' (O.xGetTag a' tPrime x') 
                        $ alts' ++ asDefault

        XCase{}         -> throw $ ErrorNotNormalized

        XCast _ _ x     -> convertBodyX pp defs x

        XType _         -> throw $ ErrorMistyped xx
        XWitness{}      -> throw $ ErrorMistyped xx



-- | Convert a function argument or let binding RHS.
convertArgX
        :: Show a 
        => Platform
        -> DataDefs L.Name
        -> Exp (AnTEC a L.Name) L.Name
        -> ConvertM a (Exp a O.Name)

convertArgX pp defs xx
  = case xx of
        -- TODO: check there are no debruijn indices.
        XVar a u        
         -> liftM2 XVar (return $ annotTail a) (convertU u)

        XCon a u
         -> case u of
                UName nCtor _   -> convertCtorAppX pp a defs nCtor []
                UPrim nCtor _   -> convertCtorAppX pp a defs nCtor []
                _               -> throw $ ErrorInvalidBound u


        -- Primitive operations.
        XApp a _ _
         | x1 : xsArgs          <- takeXApps xx
         , XVar _ UPrim{}       <- x1
         -> do  x1'     <- convertArgX pp defs x1
                xsArgs' <- mapM (convertArgX pp defs) xsArgs
                return  $ makeXApps (annotTail a) x1' xsArgs'

        -- Primitive data constructors.
        XApp a _ _
         | x1 : xsArgs            <- takeXApps xx
         , XCon _ (UPrim nCtor _) <- x1
         -> convertCtorAppX pp a defs nCtor xsArgs

        XApp a _ _
         | x1 : xsArgs            <- takeXApps xx
         , XCon _ (UName nCtor _) <- x1
         -> convertCtorAppX pp a defs nCtor xsArgs

        -- Function application
        -- TODO: This only works for full application. 
        --       At least check for the other cases.
        XApp (AnTEC _t _ _ a') _ _
         | x1 : xsArgs          <- takeXApps xx
         -> do  x1'             <- convertArgX pp defs x1

                -- We need to keep region arguments, 
                -- but can discard other types and witnesses.
                let keepArg x
                        = case x of
                                XType (TVar u)  -> isRegionKind (typeOfBound u)
                                XWitness{}      -> False
                                _               -> True

                let xsArgs_exp  = [x | x <- xsArgs
                                     , keepArg x ]

                xsArgs_exp'     <- mapM (convertArgX pp defs) xsArgs_exp
                return $ makeXApps a' x1' xsArgs_exp'

         | otherwise 
         -> error $ "toSaltX: XApp shouldn't happen as above covers all cases."


        XCast _ _ x     -> convertArgX pp defs x

        -- Lambdas, should have been split out to top-level bindintg.
        XLAM{}          -> throw ErrorNotNormalized
        XLam{}          -> throw ErrorNotNormalized

        -- Lets and cases should 
        XLet{}          -> throw ErrorNotNormalized
        XCase{}         -> throw ErrorNotNormalized 

        -- Types and witness arguments should have been discarded already.
        XType t         -> liftM XType (convertT t)
        XWitness{}      -> error "convertArgX: witness as arg" -- throw ErrorMistyped


convertCtorAppX 
        :: Show a
        => Platform 
        -> AnTEC a L.Name
        -> DataDefs L.Name
        -> L.Name
        -> [Exp (AnTEC a L.Name) L.Name]
        -> ConvertM a (Exp a O.Name)

convertCtorAppX pp a@(AnTEC t _ _ _) defs nCtor xsArgs

 -- Pass through unboxed literals.
 | L.NameBool b         <- nCtor
 , []                   <- xsArgs
 = do   t'              <- convertT t
        return $ XCon (annotTail a) (UPrim (O.NameBool b) t')

 | L.NameNat i          <- nCtor
 , []                   <- xsArgs
 = do   t'              <- convertT t
        return $ XCon (annotTail a) (UPrim (O.NameNat i) t')

 | L.NameInt i         <- nCtor
 , []                   <- xsArgs
 = do   t'              <- convertT t
        return $ XCon (annotTail a) (UPrim (O.NameInt i) t')

 | L.NameWord i bits    <- nCtor
 , []                   <- xsArgs
 = do   t'              <- convertT t
        return $ XCon (annotTail a) (UPrim (O.NameWord i bits) t')


 -- Construct algbraic data that has a finite number of data constructors.
 | Just ctorDef         <- Map.lookup nCtor $ dataDefsCtors defs
 , Just dataDef         <- Map.lookup (dataCtorTypeName ctorDef) $ dataDefsTypes defs
 = do   xsArgs'         <- mapM (convertArgX pp defs)  xsArgs

        let makeFieldType x
                = case takeAnnotOfExp x of
                        Nothing  -> return Nothing
                        Just a'  -> liftM Just $ convertT (annotType a')

        tsArgs'         <- mapM makeFieldType xsArgs
        constructData pp (annotTail a) dataDef ctorDef xsArgs' tsArgs'


convertCtorAppX _ _ _ nCtor xsArgs
 = error $ "toSaltX: convertCtorAppX failed " ++ show (nCtor, xsArgs)


-- Alt ------------------------------------------------------------------------
convertA 
        :: Show a
        => Platform
        -> DataDefs L.Name 
        -> a
        -> Bound L.Name 
        -> Alt (AnTEC a L.Name) L.Name 
        -> ConvertM a (Alt a O.Name)

convertA pp defs a uScrut alt
 = case alt of
        AAlt PDefault x
         -> do  x'      <- convertBodyX pp defs x
                return  $ AAlt PDefault x'

        -- Match against literal unboxed values.
        AAlt (PData uCtor []) x
         | UPrim nCtor _        <- uCtor
         , case nCtor of
                L.NameInt{}     -> True
                L.NameWord{}    -> True
                L.NameBool{}    -> True
                _               -> False
         -> do  uCtor'  <- convertU uCtor
                xBody1  <- convertBodyX pp defs x
                return  $ AAlt (PData uCtor' []) xBody1

        -- Match against algebraic data with a finite number
        -- of data constructors.
        AAlt (PData uCtor bsFields) x
         | Just nCtor    <- case uCtor of
                                UName n _ -> Just n
                                UPrim n _ -> Just n
                                _         -> Nothing
         , Just ctorDef   <- Map.lookup nCtor $ dataDefsCtors defs
         -> do  
                uScrut'         <- convertU uScrut

                -- Get the tag of this alternative.
                let iTag        = fromIntegral $ dataCtorTag ctorDef
                let uTag        = UPrim (O.NameTag iTag) O.tTag

                -- Get the address of the payload.
                bsFields'       <- mapM convertB bsFields

                -- Convert the right of the alternative.
                xBody1          <- convertBodyX pp defs x

                -- Add let bindings to unpack the constructor.
                xBody2          <- destructData pp a uScrut' ctorDef bsFields' xBody1

                return  $ AAlt (PData uTag []) xBody2

        AAlt{}          
         -> error $ "convertA: invalid alt " -- throw ErrorInvalidAlt



-- Data Constructor -----------------------------------------------------------
convertC :: Show a
         => DataDefs L.Name
         -> a 
         -> Bound L.Name 
         -> ConvertM a (Exp a O.Name, Type O.Name)

convertC _defs a uu
 = case uu of
        UPrim (L.NameBool v) _   
          -> return ( XCon a (UPrim (O.NameBool v)      (O.tBool))
                    , O.tBool)

        UPrim (L.NameNat i) _   
          -> return ( XCon a (UPrim (O.NameNat i) O.tNat)
                    , O.tNat)

        UPrim (L.NameInt i) _   
          -> return ( XCon a (UPrim (O.NameInt i) O.tInt)
                    , O.tInt)

        UPrim (L.NameWord i bits) _   
          -> return ( XCon a (UPrim (O.NameWord i bits) (O.tWord bits))
                    , O.tWord bits)


        _ -> error $ "convertC failed"


