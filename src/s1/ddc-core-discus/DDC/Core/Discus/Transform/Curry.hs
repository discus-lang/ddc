
module DDC.Core.Discus.Transform.Curry
        (curryModule)
where
import DDC.Core.Discus.Transform.Curry.Call
import DDC.Core.Discus.Transform.Curry.Callable
import DDC.Core.Discus.Transform.Curry.Error
import DDC.Core.Discus.Prim
import DDC.Core.Transform.Reannotate
import DDC.Core.Exp.Annot.AnTEC
import DDC.Core.Discus.Profile
import DDC.Core.Module
import DDC.Core.Exp.Annot
import Data.Maybe
import Data.Map                                 (Map)
import DDC.Core.Env.EnvT                        (EnvT)
import qualified DDC.Core.Fragment              as C
import qualified DDC.Core.Call                  as Call
import qualified Data.Map.Strict                as Map
import qualified Data.List                      as List


-- | Insert primitives to manage higher order functions in a module.
--
--   We work out which supers are being fully applied, under applied or
--   over applied, and build and evaluate closures as necessary.
--
curryModule
        :: Show a
        => Module (AnTEC a Name) Name   -- ^ Module to transform.
        -> Either Error (Module () Name)

curryModule mm
 = do
        -- Extract the top-level environment for types from the module.
        let kenv  = C.profilePrimKinds profile
        let envt  = moduleEnvT kenv mm

        -- Add all the foreign functions to the function map.
        -- We can do a saturated call for these directly.
        callables <- fmap (Map.fromList . catMaybes)
                  $  mapM (uncurry takeCallableFromImport)
                  $  moduleImportValues mm

        -- Apply curry transform in the body of the module.
        xBody'    <- curryBody envt callables
                  $  moduleBody mm

        return  $ mm { moduleBody = xBody' }


-- | Manage higher-order functions in a module body.
curryBody
        :: Show a
        => EnvT Name                    -- ^ Current type environment.
        -> Map Name Callable            -- ^ Map of directly callable supers
        -> Exp (AnTEC a Name) Name      -- ^ Expression to transform.
        -> Either Error (Exp () Name)

curryBody envt callables xx
 = case xx of
        XLet _ (LRec bxs) xBody
         -> do  let (bs, xs) = unzip bxs

                -- Add types of supers to the map of callable things.
                csSuper <- fmap (Map.fromList)
                        $  mapM (uncurry takeCallableFromSuper) bxs

                let callables'
                        = Map.union csSuper callables

                -- Rewrite bindings in the body of the let-expression.
                xs'      <- mapM (curryX envt callables') xs
                let bxs' =  zip bs xs'
                xBody'   <- curryBody envt callables' xBody
                return   $  XLet () (LRec bxs') xBody'

        _ ->    return   $ reannotate (const ()) xx


-- | Manage function application in an expression.
curryX  :: Show a
        => EnvT Name                    -- ^ Current type environment.
        -> Map Name Callable            -- ^ Map of directly callable supers.
        -> Exp (AnTEC a Name) Name      -- ^ Expression to transform.
        -> Either Error (Exp () Name)

curryX envt callables xx
 = let down x = curryX envt callables x
   in case xx of
        XVar  a (UName nF)
         -> do  result    <- makeCall envt callables nF (annotType a) []
                case result of
                 Just xx' -> return xx'
                 Nothing  -> return $ XVar () (UName nF)

        XVar  _ u
         ->     return $ XVar () u

        XApp  _ x1 a2
         -> do  result  <- curryX_call envt callables xx
                case result of
                    Just xx' -> return xx'
                    Nothing  -> XApp () <$> down x1 <*> curryArg envt callables a2

        XCast _ CastRun x1
         -> do  result  <- curryX_call envt callables xx
                case result of
                 Just xx' -> return xx'
                 Nothing  -> XCast () CastRun    <$> down x1

        -- Boilerplate.

        XAbs     _ (MType b) xBody
         ->     XAbs () (MType b) <$> curryX   envt callables  xBody

        XAbs     _ (MTerm b) xBody
         -> let callables' = shadowCallables [b] callables
            in  XAbs () (MTerm b) <$> curryX   envt callables' xBody

        XAbs     _ (MImplicit b) _
         -> Left $ ErrorSuperImplicitParams b

        XLet     _ lts@(LLet b _) xBody
         -> let callables' = shadowCallables [b] callables
            in  XLet  ()  <$> curryLts envt callables' lts
                          <*> curryX   envt callables' xBody

        XLet     _ lts@(LRec bxs) xBody
         -> let bs         = map fst bxs
                callables' = shadowCallables bs callables
            in  XLet  ()  <$> curryLts envt callables' lts
                          <*> curryX   envt callables' xBody

        XLet     _ lts@(LPrivate{}) xBody
         ->     XLet  ()  <$> curryLts envt callables  lts
                          <*> curryX   envt callables  xBody

        XAtom _ p
         ->     return $ XAtom () p

        XCase    _ x as
         ->     XCase ()  <$> down x
                          <*> mapM (curryAlt envt callables) as

        XCast    _ c xBody
         ->     XCast ()  <$> return (reannotate (const ()) c)
                          <*> curryX envt callables xBody

        XAsync _ b e1 e2
         -> let callables' = shadowCallables [b] callables
            in  XAsync () b <$> (curryX envt callables' e1)
                            <*> (curryX envt callables' e2)

curryArg  :: Show a
        => EnvT Name                    -- ^ Current type environment.
        -> Map Name Callable            -- ^ Map of directly callable supers.
        -> Arg (AnTEC a Name) Name      -- ^ Expression to transform.
        -> Either Error (Arg () Name)

curryArg envt callables arg
 = case arg of
        RType t         -> return $ RType t
        RWitness w      -> return $ RWitness $ reannotate (const ()) w
        RTerm x         -> fmap RTerm     $ curryX   envt callables x
        RImplicit arg'  -> fmap RImplicit $ curryArg envt callables arg'


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
        :: Show a
        => EnvT Name
        -> Map Name Callable
        -> Exp (AnTEC a Name) Name
        -> Either Error (Maybe (Exp () Name))

curryX_call envt callables xx
 -- If this is a call of a named function then split it into the
 --  functional part and arguments, then work out how to call it.
 | (xF, esArgs)         <- Call.takeCallElim xx
 , XVar aF (UName nF)   <- xF
 , case nF of
        NameVar{}       -> True
        NameExt{}       -> True
        _               -> False
 , length esArgs  > 0
 = do   esArgs'   <- mapM downElim esArgs
        makeCall envt callables nF (annotType aF) esArgs'

 | otherwise
 = return $ Nothing

 where  downElim ee
         = case ee of
                Call.ElimType  _ t
                 -> return $ Call.ElimType  () t

                Call.ElimValue _ x
                 ->  Call.ElimValue ()
                 <$> curryX envt callables x

                Call.ElimRun   _
                 -> return $ Call.ElimRun   ()


-- | Manage function application in a let binding.
curryLts :: Show a
         => EnvT Name
         -> Map Name Callable
         -> Lets (AnTEC a Name) Name
         -> Either Error (Lets () Name)

curryLts envt callables lts
 = case lts of
        LLet b x
         -> LLet b <$> curryX envt callables x

        LRec bxs
         -> do  let (bs, xs) =  unzip bxs
                xs'          <- mapM (curryX envt callables) xs
                return  $ LRec  $ zip bs xs'

        LPrivate bs mt ws
         -> return $ LPrivate bs mt ws


-- | Manage function application in a case alternative.
curryAlt :: Show a
         => EnvT Name
         -> Map Name Callable
         -> Alt (AnTEC a Name) Name
         -> Either Error (Alt () Name)

curryAlt envt callables alt
 = case alt of
        AAlt w xBody
         -> let bs         = bindsOfPat w
                callables' = shadowCallables bs callables
            in  AAlt w  <$> curryX envt callables' xBody

