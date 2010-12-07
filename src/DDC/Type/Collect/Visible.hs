{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
module DDC.Type.Collect.Visible
	(visibleRsT)
where
import DDC.Main.Error
import DDC.Main.Pretty
import DDC.Type.Exp
import DDC.Type.Builtin
import DDC.Type.Collect.FreeTVars
import DDC.Type.Pretty	()
import Data.Set			(Set)
import qualified Data.Set	as Set
import qualified Data.Map	as Map

stage	= "DDC.Type.Collect.Visible"


-- | Collect the list of regions that are visible (observable) in a type.
--
--   These are the regions who's effects cannot be masked because the evaluation
--   of the function affects them in a visible way.
--
--   We can't just call freeVarsT, because we don't want to get region vars present
--   in the effect portion of the type.
--
--   TODO: We really want just the material vars, replace this function
--         when material/immaterial var checking works.
-- 
visibleRsT :: Type -> Set Type
visibleRsT tt
 = case tt of
	TCon{}		-> Set.empty
	TError{}	-> Set.empty	

	TForall _ _ t	-> visibleRsT t

	TConstrain t crs 
	 -> Set.unions 
		( visibleRsT t
		: (map freeTVars $ Map.elems $ crsMore crs))


	TSum _ ts	 -> Set.unions $ map visibleRsT ts

	TVar k _
	 | k == kRegion	-> Set.singleton tt

	TVar{}		-> Set.empty

	TApp (TCon (TyConEffect{})) _
	 -> Set.empty

	TApp t1 t2
	 -> Set.unions
		[ visibleRsT t1
		, visibleRsT t2 ]
	 
	_ -> panic stage
	 	$ "visibleRsT: no match for " % tt


---------------------------------------------------------------------------------------------------

{- 	NOTE [Visible regions and effect parameters]

	In the following code from from Typing/Masking/MaskOrder3, the parameter
	function "f" is passed a worker function that updates some hidden state
	bound to "x".
	
	  fun f
 	   = do	x 	= 0
		bump () = do { x += 1; copy x }
		f bump
		()
	
	The type of "fun" is:
	
	  fun	:: forall t0 %r0 %r1 !e0 !e1 $c0 $c1
        	.  ((Unit -(!e1 $c0)> Int32 %r1) -(!e0 $c1)> t0) -(!e0)> Unit
        	:- $c0   :> ${x : %r0}
        	,  !e1   :> !Write %r0 + !Read %r0 + !Read %r1
        	,  Mutable %r0
	
	The region holding the state of "x" is %r0. Note that this region only
	appears in the effect of a parameter, and not in the body of the type, 
	yet we cannot mask it. Region %r0 is not material either, so not all 
	visible vars are material. For example, trying to suspend an application
	of the bump function should result in an error, as write effects cannot
	be purified:

 	  main ()
 	   = do b	= False
		fun $ \g -> do
			y	= g @ ()
			z	= if b then y else 5
			putStrLn $ show $ g () + z
	
	We can use this error as a check that we're not inadvertantly masking 
	effects on regions only reachable from parameter variables.
-}
