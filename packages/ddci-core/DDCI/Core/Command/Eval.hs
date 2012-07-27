
module DDCI.Core.Command.Eval
        ( cmdStep
        , cmdEval
        , evalExp)
where
import DDCI.Core.Stats.Trace
import DDCI.Core.Command.Check
import DDCI.Core.Output
import DDCI.Core.State
import DDC.Build.Language
import DDC.Core.Eval.Env
import DDC.Core.Eval.Step
import DDC.Core.Eval.Name
import DDC.Core.Transform.Reannotate
import DDC.Core.Fragment.Profile
import DDC.Core.Check
import DDC.Core.Exp
import DDC.Core.Pretty
import DDC.Core.Collect
import DDC.Type.Equiv
import DDC.Type.Subsumes
import DDC.Type.Compounds
import Control.Monad
import DDC.Core.Eval.Store              (Store)
import qualified DDC.Core.Eval.Store    as Store
import qualified Data.Set               as Set

import Data.Maybe (fromMaybe)
import qualified Data.Map		as Map
import Data.Typeable
import DDC.Core.Module (ModuleMap)


-- | Parse, check, and single step evaluate an expression.
cmdStep :: State -> Source -> String -> IO ()
cmdStep state source str
 | Bundle _ modules0 _ _ _		<- stateBundle state
 , (modules :: Maybe (ModuleMap (AnTEC () Name) Name))
				        <- gcast modules0
 , modules'				<- fromMaybe Map.empty modules
 = cmdParseCheckExp state fragmentEval modules' False source str >>= goStore 
 where
        -- Expression had a parse or type error.
        goStore Nothing
         = return ()

        -- Expression is well-typed.
        goStore (Just (x, tX, effX, cloX))
         = let  -- The evaluator doesn't accept any annotations
                x'      = reannotate (const ()) x

                -- Create the initial store.
                store   = startingStoreForExp x'

           in   goStep store x' tX effX cloX

        goStep store x tX effX cloX
         = do   _       <- forcePrint state store x tX effX cloX
                return ()


-- | Parse, check, and fully evaluate an expression.
cmdEval :: State -> Source -> String -> IO ()
cmdEval state source str
 | Bundle _ modules0 _ _ _		<- stateBundle state
 , (modules :: Maybe (ModuleMap (AnTEC () Name) Name))
				        <- gcast modules0
 , modules'				<- fromMaybe Map.empty modules
 = cmdParseCheckExp state fragmentEval modules' False source str >>= goEval
 where
    -- Expression had a parse or type error.
    goEval Nothing
     =	return ()

    -- Expression is well-typed.
    goEval (Just expr)
     =	evalExp state expr


-- | Evaluate an already parsed and type-checked expression.
--   Exported so transforms can test with it.
evalExp :: State -> (Exp a Name, Type Name, Effect Name, Closure Name) -> IO ()
evalExp state (x, tX, effX, cloX)
 = do   -- The evaluator doesn't want any annotations
        let x'    = reannotate (const ()) x

        -- Create the initial store.
	let store = startingStoreForExp x'

	-- Print starting expression.
	when (Set.member TraceEval  $ stateModes state)
	 $ outDocLn state (text "* STEP: " <> ppr x')

	-- Print starting store.
	when (Set.member TraceStore $ stateModes state)
	 $ do   putStrLn $ renderIndent $ ppr store
		outStr   state "\n"

	goStep store x'

    where
	goStep store x0
	 = do
	    mResult <- forcePrint state store x0 tX effX cloX
	    case mResult of
	      Nothing           -> return ()
	      Just (store', x0') -> goStep store' x0'
 

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
        -> Exp ()  Name 
        -> Type    Name
        -> Effect  Name
        -> Closure Name
        -> IO (Maybe (Store, Exp () Name))

forcePrint state store x tX effX cloX
 = case force store x of
        StepProgress store' x'
         -> case checkExp (configOfProfile $ fragmentProfile fragmentEval) 
                        primKindEnv primTypeEnv x' of
             Left err
              -> do 
                    -- Print intermediate expression.
                    when (Set.member TraceEval  $ stateModes state)
                     $ do outDocLn state $ text "* STEP: " <> ppr x'

                    -- Print intermediate store
                    when (Set.member TraceStore $ stateModes state)
                     $ do putStrLn $ renderIndent $ ppr store'
                          outStr   state "\n"
                
                    putStrLn $ renderIndent
                     $ vcat [ text "* OFF THE RAILS!"
                            , ppr err
                            , empty ]

                    return $ Nothing
                      
             Right (_, tX', effX', cloX')
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
                    let deathC = not $ subsumesT kClosure cloX cloX'
                    let death  = deathT || deathE || deathC

                    when deathT
                     $ putStrLn $ renderIndent
                     $ vcat [ text "* OFF THE RAILS!"
                            , ppr tX
                            , ppr tX' ]
                
                    if death 
                     then return Nothing
                     else return $ Just (store', x')
    
        StepDone
         -> do  -- Load the final expression back from the store to display.
                outDocLn state $ ppr $ traceStore store x
                return Nothing
        
        StepStuck
         -> do  outDocLn state 
                 $ vcat [ text "* STUCK!"
                        , empty]

                return Nothing

        StepStuckMistyped err
         -> do  putStrLn $ renderIndent
                 $ vcat [ ppr "* OFF THE RAILS!"
                        , ppr err
                        , empty]

                return Nothing

