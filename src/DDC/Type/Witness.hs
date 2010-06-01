{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
-- | Utilities concerning witness types.
module DDC.Type.Witness
	(inventWitnessOfClass)
where
import DDC.Main.Error
import DDC.Main.Pretty
import DDC.Type.Exp
import DDC.Type.Compounds
import DDC.Type.Kind

stage = "DDC.Type.Witness"


-- | Invent a place-holder witness that satisfies a type class constraint.
--	This is used in Desugar.ToCore when we don't know how to properly construct the
--	real witnesses yet.
inventWitnessOfClass :: Kind -> Maybe Type
inventWitnessOfClass k
	| Just (KCon kiCon s, ts)	<- takeKApps k
	, Just tcWitness		<- takeTyConWitnessOfKiCon kiCon
	= let 	-- Get the kinds of the type arguments.
		ks 	= map kindOfType ts

		-- The resulting kind guarantees the constraint.
		kResult	= makeKApps (KCon kiCon s) 
				(zipWith (\ki i -> TVar ki (UIndex i)) 
					ks 
					(reverse [0 .. length ks - 1]))
					
		k'	= makeKFuns ks kResult
		tyCon	= TyConWitness tcWitness k'
		witness	= makeTApp (TCon tyCon) ts

   	   in	Just witness

	| otherwise
	= freakout stage
		("inventWitnessOfClass: don't know how to build witness for '" % show k % "'\n")
		Nothing

