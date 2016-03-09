
module DDC.Core.Tetra.Transform.Curry
        (curryModule)
where
import DDC.Core.Tetra.Transform.Curry.Call
import DDC.Core.Tetra.Transform.Curry.Callable
import DDC.Core.Tetra.Transform.Curry.Error
import DDC.Core.Tetra.Prim
import DDC.Core.Transform.Reannotate
import DDC.Core.Annot.AnTEC
import DDC.Core.Compounds
import DDC.Core.Module
import DDC.Core.Exp
import Data.Maybe
import Data.Map                                 (Map)
import qualified DDC.Core.Call                  as Call
import qualified Data.Map.Strict                as Map
import qualified Data.List                      as List


-- | Insert primitives to manage higher order functions in a module.
--
--   We work out which supers are being fully applied, under applied or
--   over applied, and build and evaluate closures as necessary.
--
curryModule 
        :: Module (AnTEC a Name) Name 
        -> Either Error (Module () Name)

curryModule mm
 = do
        -- Add all the foreign functions to the function map.
        -- We can do a saturated call for these directly.
        callables <- fmap (Map.fromList . catMaybes)
                  $  mapM (uncurry takeCallableFromImport)
                  $  moduleImportValues mm

        -- Apply curry transform in the body of the module.
        xBody'    <- curryBody callables
                  $  moduleBody mm

        return  $ mm { moduleBody = xBody' }


-- | Manage higher-order functions in a module body.
curryBody 
        :: Map Name Callable
        -> Exp (AnTEC a Name) Name 
        -> Either Error (Exp () Name)

curryBody callables xx
 = case xx of
        XLet _ (LRec bxs) xBody
         -> do  let (bs, xs) = unzip bxs

                -- Add types of supers to the map of callable things.
                csSuper <- fmap (Map.fromList)
                        $  mapM (uncurry takeCallableFromSuper) bxs

                let callables'  
                        = Map.union csSuper callables

                -- Rewrite bindings in the body of the let-expression.
                xs'      <- mapM (curryX callables') xs
                let bxs' =  zip bs xs'
                xBody'   <- curryBody callables' xBody
                return   $  XLet () (LRec bxs') xBody'

        _ ->    return   $ reannotate (const ()) xx


-- | Manage function application in an expression.
curryX  :: Map Name Callable
        -> Exp (AnTEC a Name) Name 
        -> Either Error (Exp () Name)

curryX callables xx
 = let down x = curryX callables x
   in case xx of
        XVar  a (UName nF)
         -> do  result  <- makeCall callables nF (annotType a) []
                case result of 
                 Just xx' -> return xx'
                 Nothing  -> return $ XVar () (UName nF)

        XVar  _ u
         ->     return $ XVar () u

        XApp  _ x1 x2
         -> do  result  <- curryX_call callables xx
                case result of
                 Just xx' -> return xx'
                 Nothing  -> XApp () <$> down x1 <*> down x2

        XCast _ CastRun x1
         -> do  result  <- curryX_call callables xx
                case result of
                 Just xx' -> return xx'
                 Nothing  -> XCast () CastRun    <$> down x1

        -- Boilerplate.
        XCon     _ c     
         -> return $ XCon     () c

        XLam     _ b xBody   
         -> let callables' = shadowCallables [b] callables
            in  XLam () b <$> curryX   callables' xBody

        XLAM     _ b xBody
         ->     XLAM () b <$> curryX   callables  xBody

        XLet     _ lts@(LLet b _) xBody
         -> let callables' = shadowCallables [b] callables
            in  XLet  ()  <$> curryLts callables' lts 
                          <*> curryX   callables' xBody

        XLet     _ lts@(LRec bxs) xBody
         -> let bs         = map fst bxs
                callables' = shadowCallables bs callables
            in  XLet  ()  <$> curryLts callables' lts
                          <*> curryX   callables' xBody

        XLet     _ lts@(LPrivate{}) xBody
         ->     XLet  ()  <$> curryLts callables  lts
                          <*> curryX   callables  xBody

        XCase    _ x as
         ->     XCase ()  <$> down x
                          <*> mapM (curryAlt callables) as

        XCast    _ c xBody
         ->     XCast ()  <$> return (reannotate (const ()) c)
                          <*> curryX callables xBody

        XType    _ t
         -> return $ XType    () t

        XWitness _ w
         -> return $ XWitness () (reannotate (const ()) w)


-- If we introduce a locally bound name with the same name as one of
-- the top-level callable things then we need to remove it from the map
-- of callables. References in the new context refer to the local thing
-- instead.
shadowCallables :: [Bind Name] -> Map Name Callable -> Map Name Callable
shadowCallables bs callables
        = List.foldl' (flip Map.delete) callables
        $ mapMaybe takeNameOfBind bs


-- | Build a function call for the given application expression.
curryX_call 
        :: Map Name Callable
        -> Exp (AnTEC a Name) Name 
        -> Either Error (Maybe (Exp () Name))

curryX_call callables xx

 -- If this is a call of a named function then split it into the
 --  functional part and arguments, then work out how to call it.
 | (xF, esArgs)         <- Call.takeCallElim xx
 , XVar aF (UName nF)   <- xF
 , length esArgs  > 0
 = do   esArgs'   <- mapM downElim esArgs
        makeCall callables nF (annotType aF) esArgs'

 | otherwise
 = return $ Nothing

 where  downElim ee
         = case ee of
                Call.ElimType  _ _ t 
                 -> return $ Call.ElimType  () () t

                Call.ElimValue _ x   
                 ->  Call.ElimValue () 
                 <$> curryX callables x

                Call.ElimRun   _
                 -> return $ Call.ElimRun   ()


-- | Manage function application in a let binding.
curryLts :: Map Name Callable 
         -> Lets (AnTEC a Name) Name 
         -> Either Error (Lets () Name)

curryLts callables lts
 = case lts of
        LLet b x
         -> LLet b <$> curryX callables x

        LRec bxs          
         -> do  let (bs, xs) =  unzip bxs
                xs'          <- mapM (curryX callables) xs
                return  $ LRec  $ zip bs xs'

        LPrivate bs mt ws 
         -> return $ LPrivate bs mt ws


-- | Manage function application in a case alternative.
curryAlt :: Map Name Callable 
         -> Alt (AnTEC a Name) Name 
         -> Either Error (Alt () Name)

curryAlt callables alt
 = case alt of
        AAlt w xBody
         -> let bs         = bindsOfPat w
                callables' = shadowCallables bs callables
            in  AAlt w  <$> curryX callables' xBody

