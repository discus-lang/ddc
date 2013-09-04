
module DDC.Core.Check.Exp.Cast
        (checkCast)
where
import DDC.Core.Check.Exp.Base
import qualified DDC.Type.Sum   as Sum
import qualified Data.Set       as Set


checkCast :: Checker a n

-- type cast -------------------------------------
-- Weaken an effect, adding in the given terms.
checkCast !table !kenv !tenv xx@(XCast a (CastWeakenEffect eff) x1) _
 = do   let config      = tableConfig table

        -- Check the body.
        (x1', t1, effs, clo)
                        <- tableCheckExp table table kenv tenv x1 Nothing

        -- Check the effect term.
        (eff', kEff)    <- checkTypeM config kenv eff 

        -- The effect term must have Effect kind.
        when (not $ isEffectKind kEff)
         $ throw $ ErrorWeakEffNotEff xx eff' kEff

        let c'  = CastWeakenEffect eff'

        returnX a
                (\z -> XCast z c' x1')
                t1
                (Sum.insert eff' effs)
                clo


-- Weaken a closure, adding in the given terms.
checkCast !table !kenv !tenv (XCast a (CastWeakenClosure xs) x1) _
 = do   
        -- Check the body.
        (x1', t1, effs, clos)
                <- tableCheckExp table table kenv tenv x1 Nothing

        -- Check the contained expressions.
        (xs', closs)
                <- liftM unzip
                $ mapM (\x -> checkArgM table kenv tenv x Nothing) xs

        let c'  = CastWeakenClosure xs'

        returnX a
                (\z -> XCast z c' x1')
                t1
                effs
                (Set.unions (clos : closs))


-- Purify an effect, given a witness that it is pure.
checkCast !table !kenv !tenv xx@(XCast a (CastPurify w) x1) _
 = do   let config      = tableConfig table

        -- Check the body.
        (x1', t1, effs, clo)
                  <- tableCheckExp table table kenv tenv x1 Nothing

        -- Check the witness.
        (w', tW)  <- checkWitnessM config kenv tenv w
        let wTEC  = reannotate fromAnT w'

        -- The witness must have type (Pure e), for some effect e.
        effs' <- case tW of
                  TApp (TCon (TyConWitness TwConPure)) effMask
                    -> return $ Sum.delete effMask effs
                  _ -> throw  $ ErrorWitnessNotPurity xx w tW

        let c'  = CastPurify wTEC

        returnX a
                (\z -> XCast z c' x1')
                t1 effs' clo


-- Forget a closure, given a witness that it is empty.
checkCast !table !kenv !tenv xx@(XCast a (CastForget w) x1) _
 = do   let config      = tableConfig table

        -- Check the body.
        (x1', t1, effs, clos)  
                  <- tableCheckExp table table kenv tenv x1 Nothing

        -- Check the witness.
        (w', tW)  <- checkWitnessM config kenv tenv w        
        let wTEC  = reannotate fromAnT w'

        -- The witness must have type (Empty c), for some closure c.
        clos' <- case tW of
                  TApp (TCon (TyConWitness TwConEmpty)) cloMask
                    -> return $ maskFromTaggedSet 
                                        (Sum.singleton kClosure cloMask)
                                        clos

                  _ -> throw $ ErrorWitnessNotEmpty xx w tW

        let c'  = CastForget wTEC

        returnX a
                (\z -> XCast z c' x1')
                t1 effs clos'


-- Suspend a computation,
-- capturing its effects in a computation type.
checkCast !table !kenv !tenv (XCast a CastSuspend x1) _
 = do   
        -- Check the body.
        (x1', t1, effs, clos) 
                <- tableCheckExp table table kenv tenv x1 Nothing

        -- The result type is (S effs a),
        --  where effs is the type of the body.
        let tS  = tApps (TCon (TyConSpec TcConSusp))
                        [TSum effs, t1]

        returnX a
                (\z -> XCast z CastSuspend x1')
                tS (Sum.empty kEffect) clos


-- Run a suspended computation,
-- releasing its effects into the environment.
checkCast !table !kenv !tenv xx@(XCast a CastRun x1) _
 = do   
        -- Check the body.
        (x1', t1, effs, clos) 
                <- tableCheckExp table table kenv tenv x1 Nothing

        -- The body must have type (S eff a),
        --  and the result has type 'a' while unleashing effect 'eff'.
        case t1 of
         TApp (TApp (TCon (TyConSpec TcConSusp)) eff2) tA 
          -> returnX a
                (\z -> XCast z CastRun x1')
                tA 
                (Sum.union effs (Sum.singleton kEffect eff2))
                clos

         _ -> throw $ ErrorRunNotSuspension xx t1

checkCast _ _ _ _ _
        = error "ddc-core.checkCast: no match"


-- Arg ------------------------------------------------------------------------
-- | Like `checkExp` but we allow naked types and witnesses.
checkArgM 
        :: (Show n, Pretty n, Ord n)
        => Table a n             -- ^ Static config.
        -> Env n                -- ^ Kind environment.
        -> Env n                -- ^ Type environment.
        -> Exp a n              -- ^ Expression to check.
        -> Maybe (Type n)       -- ^ Expected type, if any.
        -> CheckM a n 
                ( Exp (AnTEC a n) n
                , Set (TaggedClosure n))

checkArgM !table !kenv !tenv !xx !_
 = case xx of
        XType t
         -> do  checkTypeM (tableConfig table) kenv t
                let Just clo = taggedClosureOfTyArg kenv t

                return  ( XType t
                        , clo)

        XWitness w
         -> do  (w', _) <- checkWitnessM (tableConfig table) kenv tenv w
                return  ( XWitness (reannotate fromAnT w')
                        , Set.empty)

        _ -> do
                (xx', _, _, clos) 
                        <- tableCheckExp table table kenv tenv xx Nothing
                return  ( xx'
                        , clos)


