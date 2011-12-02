
-- | Collect all free variables in this type, along with their materiality.
--   For example:
--      Bool %r1               => [(%r1, MaterialStrong)]
--      (Int %r1 -> Int %r2)   => [(%r1, MaterialNot), (%r2, MaterialNot)]
-- 
module DDC.Type.Collect.Material ()
where

{-	( freeMaterialityT
	, freeRawMaterialityT)
where
import DDC.Type.Exp
import DDC.Type.Kind
import DDC.Type.Data.Base
import DDC.Type.Compounds
import DDC.Main.Error
import Data.Map			(Map)
import qualified Data.MapUtil	as Map

stage	= "DDC.Type.Collect.FreeMaterial"

freeMaterialityT :: Type -> Map Type Materiality
freeMaterialityT tt = undefined


-- | Like `freeMateriality` but return the materiality of every free occurrance of a variable.
freeRawMaterialityT :: Type -> Map Type [Materiality]
freeRawMaterialityT tt
 = case tt of
	TNil
	 -> Map.empty

	TVar k b
	 | isRegionKind k -> Map.singleton tt [MaterialStrong]
	 | isValueKind k  -> Map.singleton tt [MaterialStrong]
	 | otherwise	  -> Map.singleton tt [MaterialNot]
	
	TCon{}	
	 -> Map.empty
	
	TSum k ts
	 -> Map.unionsWith (++) 
	 $  map (freeRawMaterialityT) ts

	-- For a type like (Int %r1 -> Int %r2), even though %r1 is strongly material
	-- in Int %r1, in the function type it is in an immaterial context, so is
	-- immaterial in the overall function type.
	TApp t1 t2
	 |  Just (tc, ts)	<- takeTDataTC tt
	 ,  TyConData { tyConDataDef = Just dataDef } <- tc
	 -> let	tsRawMat	= Map.map freeRawMaterialityT ts
	 	Just msParam	= paramMaterialityOfDataDef dataDef
	    in	zipWith contextualiseMapMateriality msParam tsRawMat
		
	 | Just (t1', t2', tEff, tClo) <- takeTFun tt
	 -> let	tsRawMat	= map freeRawMaterialityT [t1', t2', tEff, tClo]
		msParam		= paramMaterialityOfFun
	    in	zipWith contextualiseMapMateriality msParam tsRawMat

	 -- For abstract types, we don't know what they will be instantiated as. 
	 -- However, it's always safe to assume the context is strongly material, and 
	 -- the contextualising a matriality using a strongly material context is a no-op, 
	 -- so we can just pass the raw materialities of the types through unharmed.
	 | otherwise
	 -> 	map freeRawMaterialityT [t1, t2]	

	TForall _ _ t
	 -> panic stage "finish me"
	
	TConstrain t crs
	 -> panic stage "finish me"
	
	TError 
	 -> Map.empty
	

-- | Material vars in immaterial contexts are still immaterial.
contextualiseMapMateriality
	:: Materiality
	-> Map Type [Materiality] 
	-> Map Type [Materiality]

contextualiseMapMateriality mat ms
	= Map.map (map (contextualiseMateriality mat)) ms
-}
	
