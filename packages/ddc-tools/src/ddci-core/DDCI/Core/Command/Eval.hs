
module DDCI.Core.Command.Eval
        ( cmdStep
        , cmdEval
        , evalExp)
where
import DDCI.Core.Stats.Trace
import DDCI.Core.Output
import DDCI.Core.State
import DDC.Driver.Command.Check
import DDC.Core.Eval.Env
import DDC.Core.Eval.Step
import DDC.Core.Eval.Name
import DDC.Core.Transform.Reannotate
import DDC.Core.Exp
import DDC.Core.Check
import DDC.Core.Pretty
import DDC.Core.Collect
import DDC.Core.Compounds
import DDC.Type.Equiv
import DDC.Type.Subsumes
import Control.Monad
import DDC.Core.Eval.Store                      (Store)
import qualified DDC.Core.Eval.Store            as Store
import qualified Data.Set                       as Set
import qualified DDC.Build.Language.Eval        as Eval
import qualified Data.Map		     as Map
import Data.Typeable
import DDC.Core.Module (ModuleMap)
import Data.Maybe (fromMaybe)


-- | Parse, check, and single step evaluate an expression.
cmdStep :: State -> Source -> String -> IO ()
cmdStep state source str
 | Language bundle      <- stateLanguage state
 , modules0             <- bundleModules bundle
 , (modules :: Maybe (ModuleMap (AnTEC () Name) Name))
			<- gcast modules0
 , modules'		<- fromMaybe Map.empty modules
 =   cmdParseCheckExp Eval.fragment modules' False Recon source str 
 >>= goStore 
 where
        -- Expression is well-typed.
        goStore (Just x, _)
         = let  -- The evaluator doesn't accept any annotations
                x_stripped = reannotate (const ()) x

                -- Create the initial store.
                store      = startingStoreForExp x_stripped

           in   goStep store x

        -- Expression had a parse or type error.
        goStore _
         = return ()
        
        goStep store x
         = do   _       <- forcePrint state store x
                return ()


-- | Parse, check, and fully evaluate an expression.
cmdEval :: State -> Source -> String -> IO ()
cmdEval state source str
 | Language bundle      <- stateLanguage state
 , modules0             <- bundleModules bundle
 , (modules :: Maybe (ModuleMap (AnTEC () Name) Name))
			<- gcast modules0
 , modules'		<- fromMaybe Map.empty modules
 =   cmdParseCheckExp Eval.fragment modules' False Recon source str 
 >>= goEval
 where
    -- Expression is well-typed.
    goEval (Just expr, _)
     =	evalExp state 
     $  reannotate (\a -> a { annotTail = ()} ) expr

    -- Expression had a parse or type error.
    goEval _
     =  return ()



-- | Evaluate an already parsed and type-checked expression.
--   Exported so transforms can test with it.
evalExp :: State -> Exp (AnTEC () Name) Name -> IO ()
evalExp state x0
 = do   
        -- The evaluator doesn't want any annotations
        let x0_stripped = reannotate (const ()) x0
        let x0_zapped   = reannotate (\a -> a { annotTail = ()}) x0

        -- Create the initial store.
	let store = startingStoreForExp x0_stripped

	-- Print starting expression.
	when (Set.member TraceEval  $ stateModes state)
	 $ outDocLn state (text "* STEP: " <> ppr x0_stripped)

	-- Print starting store.
	when (Set.member TraceStore $ stateModes state)
	 $ do   putStrLn $ renderIndent $ ppr store
		outStr   state "\n"

	goStep store x0_zapped

    where
	goStep store x
	 = do mResult <- forcePrint state store x
	      case mResult of
	       Nothing            
                -> return ()

	       Just (store', x') 
                -> goStep store' x'
 

-- | Create a starting store for the given expression.
--   We pre-allocate any region handles in the expression, 
--   and ensure that the next region to be allocated is higher
--   than all of these.
startingStoreForExp :: Exp () Name -> Store
startingStoreForExp xx
 =  let
        -- Gather up all the region handles already in the expression.
        rs      = [ r | UPrim (NameRgn r) _ <- Set.toList $ collectBound xx]

        -- Decide what new region should be allocated first.
        -- Region 0 is reserved for thunks.
        iAllocBump
         | _ : _         <- rs
         , Rgn iR        <- maximum rs
         = iR

         | otherwise
         = 0

        store  = Store.initial

    in  store
        { Store.storeNextRgn = Store.storeNextRgn store + iAllocBump
        , Store.storeRegions = Store.storeRegions store `Set.union` Set.fromList rs
        , Store.storeGlobal  = Store.storeGlobal  store `Set.union` Set.fromList rs }


-- | Perform a single step of evaluation and print what happened.
forcePrint 
        :: State
        -> Store 
        -> Exp (AnTEC a Name) Name 
        -> IO (Maybe (Store, Exp (AnTEC () Name) Name))

forcePrint state store x
 = let  
        -- Get the type, effect and closure of the original expression.
        annot   = annotOfExp x
        tX      = annotType annot
        effX    = annotEffect annot
        cloX    = annotClosure annot

        -- The evaluator doesn't want any type annotations on the expresison.
        x_stripped = reannotate (const ()) x

   in case force store x_stripped of
        StepProgress store' x_stripped'
         -> case fst $ checkExp (configOfProfile Eval.profile) 
                        primKindEnv primTypeEnv 
                        x_stripped' Recon of
             Left err
              -> do 
                    -- Print intermediate expression.
                    when (Set.member TraceEval  $ stateModes state)
                     $ do outDocLn state $ text "* STEP: " <> ppr x_stripped'

                    -- Print intermediate store
                    when (Set.member TraceStore $ stateModes state)
                     $ do putStrLn $ renderIndent $ ppr store'
                          outStr   state "\n"
                
                    putStrLn $ renderIndent
                     $ vcat [ text "* OFF THE RAILS!"
                            , ppr err
                            , empty ]

                    return $ Nothing
                      
             Right (x', tX', effX', cloX')
              -> do 
                    -- Print intermediate expression.
                    when (Set.member TraceEval  $ stateModes state)
                     $ do outDocLn state $ text "* STEP: " <> ppr x'

                    -- Print intermediate store
                    when (Set.member TraceStore $ stateModes state)
                     $ do putStrLn $ renderIndent $ ppr store'
                          putStr "\n"
                
                    -- Check expression has the same type as before,
                    -- and that the effect and closure are no greater.
                    let deathT = not $ equivT    tX tX'
                    let deathE = not $ subsumesT kEffect  effX effX'

                    -- ISSUE #290: locations don't reveal closure information.
--                  let deathC = not $ subsumesT kClosure cloX cloX'
                    let death  = deathT || deathE -- deathC


                    when death
                     $ putStrLn $ renderIndent
                     $ vcat [ empty
                            , text "* OFF THE RAILS! ************************"
                            , text "Starting expression:" 
                            , indent 4 $ ppr x
                            , empty
                            , text "Has type:",    indent 4 $ ppr tX
                            , text "Has efect:",   indent 4 $ ppr effX
                            , text "Has closure:", indent 4 $ ppr cloX
                            , empty
                            , text "Reduced expression:"
                            , indent 4 $ ppr x'
                            , empty
                            , text "Has type:",    indent 4 $ ppr tX'
                            , text "Has effect:",  indent 4 $ ppr effX'
                            , text "Has closure:", indent 4 $ ppr cloX' ]

                    if death 
                     then return Nothing
                     else return $ Just (store', x')
    
        StepDone
         -> do  -- Load the final expression back from the store to display.
                outDocLn state $ ppr $ traceStore store x_stripped
                return Nothing
        
        StepStuck
         -> do  outDocLn state 
                 $ vcat [ text "* STUCK!"
                        , empty]

                return Nothing

        StepMistyped err
         -> do  putStrLn $ renderIndent
                 $ vcat [ text "* OFF THE RAILS!"
                        , ppr err
                        , empty]

                return Nothing

