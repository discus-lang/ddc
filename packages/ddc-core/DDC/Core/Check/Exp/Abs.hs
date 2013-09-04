
module DDC.Core.Check.Exp.Abs
        (checkAbs)
where
import DDC.Core.Check.Exp.Base
import qualified DDC.Type.Sum   as Sum
import qualified DDC.Type.Env   as Env
import qualified Data.Set       as Set


checkAbs :: Checker a n

-- spec abstraction -----------------------------
checkAbs !table !kenv !tenv xx@(XLAM a b1 x2) _
 = do   let config      = tableConfig table
        
        -- Check the type of the binder.
        (b1', _)        <- checkBindM config kenv b1

        -- The bound variable cannot shadow others in the environment.
        when (Env.memberBind b1' kenv)
         $ throw $ ErrorLamShadow xx b1
        
        -- Check the body of the abstraction.
        let kenv'       = Env.extend b1' kenv
        let tenv'       = Env.lift   1  tenv
        (x2', t2, e2, c2) 
                <- tableCheckExp table table  kenv' tenv' x2 Nothing
        (_, k2) <- checkTypeM config kenv' t2

        -- The body of a spec abstraction must have data kind.
        when (not $ isDataKind k2)
         $ throw $ ErrorLamBodyNotData xx b1 t2 k2

        -- The body of a spec abstraction must be pure.
        when (e2 /= Sum.empty kEffect)
         $ throw $ ErrorLamNotPure xx UniverseSpec (TSum e2)

        -- Mask closure terms due to locally bound region vars.
        let c2_cut      = Set.fromList
                        $ mapMaybe (cutTaggedClosureT b1)
                        $ Set.toList c2

        returnX a
                (\z -> XLAM z b1' x2')
                (TForall b1' t2)
                (Sum.empty kEffect)
                c2_cut
         

-- function abstraction -------------------------
checkAbs !table !kenv !tenv xx@(XLam a b1 x2) _
 = do   let config      = tableConfig table

        -- Check the type of the binder.
        (b1', k1)       <- checkBindM config kenv b1
        let t1          = typeOfBind b1'
        
        -- Check the body of the abstraction.
        let tenv'       = Env.extend b1' tenv
        (x2', t2, e2, c2) 
                <- tableCheckExp table table kenv tenv' x2 Nothing

        -- The typing rules guarantee that the checked type of an 
        -- expression is well kinded, but we need to check it again
        -- to find out what that kind is.
        (_, k2) <- checkTypeM config kenv t2

        -- The form of the function constructor depends on what universe the 
        -- binder is in.
        case universeFromType2 k1 of

         -- This is a data abstraction.
         Just UniverseData

          -- The body of a data abstraction must accept data.
          |  not $ isDataKind k1
          -> throw $ ErrorLamBindNotData xx t1 k1

          -- The body of a data abstraction must produce data.
          |  not $ isDataKind k2
          -> throw $ ErrorLamBodyNotData xx b1 t2 k2 

          -- Looks good.
          |  otherwise
          -> let 
                 -- Cut closure terms due to locally bound value vars.
                 -- This also lowers deBruijn indices in un-cut closure terms.
                 c2_cut  = Set.fromList
                         $ mapMaybe (cutTaggedClosureX b1)
                         $ Set.toList c2

                 -- Trim the closure before we annotate the returned function
                 -- type with it. This should always succeed because trimClosure
                 -- only returns Nothing if the closure is miskinded, and we've
                 -- already already checked that.
                 Just c2_captured

                  -- If we're not tracking closure information then just drop it 
                  -- on the floor.
                  | not  $ configTrackedClosures config
                  = Just $ tBot kClosure

                  | otherwise
                  = trimClosure $ closureOfTaggedSet c2_cut

                 -- If we're not tracking effect information then just drop it 
                 -- on the floor.
                 e2_captured
                  | not  $ configTrackedEffects config
                  = tBot kEffect

                  | otherwise
                  = TSum e2

                 -- If the function type for the current fragment supports
                 -- latent effects and closures then just use that.
                 fun_result
                  | configFunctionalEffects  config
                  , configFunctionalClosures config
                  = returnX a
                        (\z -> XLam z b1 x2')
                        (tFunEC t1 e2_captured c2_captured t2)
                        (Sum.empty kEffect)
                        c2_cut

                 -- If the function type for the current fragment does not
                 -- support latent effects, then the body expression needs
                 -- to be pure.
                  | e2_captured == tBot kEffect
                  , c2_captured == tBot kClosure
                  = returnX a
                        (\z -> XLam z b1 x2')
                        (tFun t1 t2)
                        (Sum.empty kEffect)
                        Set.empty

                  | e2_captured /= tBot kEffect
                  = throw $ ErrorLamNotPure  xx UniverseData e2_captured

                  | c2_captured /= tBot kClosure
                  = throw $ ErrorLamNotEmpty xx UniverseData c2_captured

                  -- One of the above error cases is supposed to fire,
                  -- so we should never hit this error.
                  | otherwise
                  = error "checkExpM': can't build function type."

             in  fun_result


         -- This is a witness abstraction.
         Just UniverseWitness

          -- The body of a witness abstraction must be pure.
          | e2 /= Sum.empty kEffect  
          -> throw $ ErrorLamNotPure  xx UniverseWitness (TSum e2)

          -- The body of a witness abstraction must produce data.
          | not $ isDataKind k2      
          -> throw $ ErrorLamBodyNotData xx b1 t2 k2

          -- Looks good.
          | otherwise                
          ->    returnX a
                        (\z -> XLam z b1' x2')
                        (tImpl t1 t2)
                        (Sum.empty kEffect)
                        c2

         _ -> throw $ ErrorMalformedType xx k1

-- others ---------------------------------------
checkAbs _ _ _ _ _
        = error "ddc-core.checkAbs: no match"
