
-- | Snip user provided dictionary functions to top level.
module DDC.Desugar.Projections.Snip
	( snipProjDictTree )
where
import DDC.Source.Error
import DDC.Desugar.Bits
import DDC.Desugar.Projections.Base
import DDC.Desugar.Projections.Naming
import DDC.Desugar.Exp
import DDC.Base.SourcePos
import DDC.Type
import DDC.Type.SigMode
import DDC.Var
import DDC.Util.FreeVars
import qualified Data.MapUtil		as Map
import qualified Data.Set		as Set
import qualified Shared.VarUtil		as Var
import Util


-- | Snip out functions and sigs from projection dictionaries to top level.
--   Also snip class instances while we're here.
snipProjDictTree
	:: ModuleId 			-- ^ the name of the current module
	-> Map Var (Top SourcePos)	-- ^ class dictionary definitions
	-> Tree SourcePos
	-> ProjectM (Tree SourcePos)

snipProjDictTree modName classDicts tree
 	= liftM concat
 	$ mapM (snipProjDictP modName classDicts) tree


-- | Snip RHS of bindings in projection dictionaries.
snipProjDictP modName classDicts (PProjDict sp t ss)
 = do
	let (Just (vCon, _, _))	= takeTData t

	-- See what vars are in the dict and make a map of new vars.
 	let dictVs	= Set.toList
			$ Set.unions
			$ map bindingVarsOfStmt ss

	dictVsNew 	<- mapM (newProjFunVar sp modName vCon) dictVs
	let varMap	= Map.fromList $ zip dictVs dictVsNew

	let (mpp, mss')	= unzip $ map (snipProjDictS varMap) ss

	return	$ PProjDict sp t (catMaybes mss')
		: catMaybes mpp


-- Snip RHS of bindings in type class instances.
snipProjDictP modName classDicts
	pInst@(PClassInst sp vClass ts ssInst)

	-- lookup the class definition for this instance
	| Just pClass	<- Map.lookup vClass classDicts
	= do	(ss', pss)	<- liftM unzip
				$  mapM (snipInstBind modName pClass pInst) ssInst

		return	$ PClassInst sp vClass ts ss'
			: concat pss

	| otherwise
	= do	addError $ ErrorUndefinedVar vClass
		return $ [pInst]

snipProjDictP _ _ pp
 =	return [pp]



-- | For each binding in a type class instance, rename the
--   binding and shift it to top level.
--   eg:
--	class Show a where
--	 show :: TYPE
--
--	instance Show Bool where
--	 show = EXP
--
--   yields:
--	class Show a where
--	 show :: TYPE
--
--	instance Show Bool where
--	 show = instance_Show_Bool
--
--	instance_Show_Bool :: forall a. TYPE
--	instance_Show_Bool =  EXP
--
snipInstBind
	:: ModuleId
	-> Top SourcePos		-- ^ the class dict def of this instance
	-> Top SourcePos		-- ^ the class dict instance
	-> Stmt SourcePos		-- ^ the binding in this instance to snip
	-> ProjectM ( Stmt SourcePos
		    , [Top SourcePos])

-- if the RHS is already a var we can leave it as it is.
snipInstBind modName
	pClass pInst
	bind@(SBind spBind (Just vInst) (XVar{}))
 = 	return (bind, [])

-- otherwise lift it out to top level
snipInstBind modName
	pDict@(PClassDecl _  vClass  tsClass vtsClass)
	pInst@(PClassInst _  _       tsInst  _)
	sBind@(SBind sp (Just vInst) _)
 = do
	-- create a new top-level variable to use for this binding
 	vTop	<- newInstFunVar sp modName vClass tsInst vInst

	-- lookup the type for this instance function and substitute
	--	in the types for this instance
	case lookup vInst vtsClass of
	 Nothing
	  -> do	addError $ ErrorNotMethodOfClass vInst vClass
		return (sBind, [])

	 -- instance function is not defined in the type class declaration
	 Just tInst
	  -> snipInstBind' modName pDict pInst sBind vTop tInst


-- | Make the type signature for the instance function.
--   We also need to quantify over any free variables in the class arguments
--
--   eg for:
--	instance Int %r1 where
--	  (+) = ...
--
--   the type signature for for (+) is
--	(+) :: forall %r1 . ...
--
snipInstBind' modName
	pDict@(PClassDecl _  vClass  tsClass vtsClass)
	pInst@(PClassInst sp vClass' tsInst  ssInst)
	sBind@(SBind spBind (Just vInst) xx)
	vTop
	tInst
 = do
	let tInst_sub	= subTT_noLoops
				(Map.fromList $ zip tsClass tsInst)
				tInst

	let vsFree	= Set.filter (\v -> not $ Var.isCtorName v) $ freeVars tsInst
	let bks_quant	= map (\v -> (BVar v, let Just k = kindOfSpace $ varNameSpace v in k))
			$ Set.toList vsFree
	let tInst_quant	= makeTForall_back bks_quant tInst_sub

	-- As we're duplicating information from the original signature
	-- we need to rewrite the binders on FWhere fetters.
	-- It'd probably be nicer to use exists. quantifiers for this instead.
	tInst_fresh	<- freshenCrsEq modName tInst_quant

	return	(  SBind spBind (Just vInst) (XVar spBind vTop)
		,  [ PTypeSig spBind SigModeMatch [vTop] tInst_fresh
		   , PBind    spBind  vTop  xx])


-- | Snip the RHS of this statement down to a var
snipProjDictS
	:: Map Var Var
	-> Stmt a
	-> ( Maybe (Top a)
	   , Maybe (Stmt a))

snipProjDictS varMap xx
	| SBind nn (Just v) x	<- xx
	, Just v'		<- Map.lookup v varMap
	= ( Just $ PBind nn v' x
	  , Just $ SBind nn (Just v)  (XVar nn v'))

	| SSig  nn sigMode vs t	 <- xx
	, Just vs'		<- sequence $ map (\v -> Map.lookup v varMap) vs
	= ( Just $ PTypeSig nn sigMode vs' t
	  , Nothing )

	| otherwise
	= ( Nothing
	  , Just xx)
