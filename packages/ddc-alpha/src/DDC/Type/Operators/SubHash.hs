
{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Type substitution using a hashtable.
module DDC.Type.Operators.SubHash
	(subHashVT_noLoops)
where
import DDC.Type.Compounds
import DDC.Type.Exp
import DDC.Var
import DDC.Main.Error
import Control.Monad
import Data.HashTable.IO                (LinearHashTable)
import Data.Set                         (Set)
import qualified Data.Set               as Set
import qualified Data.HashTable.IO      as Hash

stage	= "DDC.Type.Operators.SubHash"

-- | Substitute variables for types in some type.
--   Recursive substutions cause 'panic'.
--   TODO: finsih for UMore, TForall and TConstrain 
subHashVT_noLoops
	:: LinearHashTable Var Type
	-> Type
	-> IO Type

subHashVT_noLoops table tt
	= subVT table Set.empty tt

	
subVT 	:: LinearHashTable Var Type
	-> Set Var
	-> Type
	-> IO Type
	
subVT table vsSubbed tt
 = let	down = subVT table vsSubbed
   in  case tt of
	TNil	-> return TNil

	TVar _ u
	 -> case u of
		UVar v
		 -> do	mRight	<- Hash.lookup table v
			case mRight of
			 Nothing	-> return tt
			 Just right	
			  | Set.member v vsSubbed
			  -> panic stage $ "subVT: recursive substitution"
		
			  | otherwise
			  -> subVT table (Set.insert v vsSubbed) right

		UMore{}		-> panic stage $ "subVT: not finished"
		UIndex{}	-> return tt
		UClass{}	-> return tt
		
	TCon{}		-> return tt
	TSum    k ts	-> liftM2 makeTSum (return k) (mapM down ts)
	TApp    t1 t2	-> liftM2 TApp (down t1)  (down t2)
	TForall{}	-> panic stage $ "subVT: not finished"
	TConstrain{}	-> panic stage $ "subVT: not finished"
	TError{}	-> return tt

