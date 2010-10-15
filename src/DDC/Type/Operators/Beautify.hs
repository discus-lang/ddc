{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Beautification of the names in a type.
module DDC.Type.Operators.Beautify
	( beautifyNamesT
	, beautifyLocalNamesT)
where
import DDC.Type.Operators.Substitute
import DDC.Type.Collect
import DDC.Type.Compounds
import DDC.Type.Exp
import DDC.Var.NiceNames
import Data.List
import Data.Maybe
import qualified Data.Foldable	as Foldable
import qualified Data.Set	as Set

-- | Beautify all the names in a type, including names of unquantified vars.
beautifyNamesT :: Type -> Type
beautifyNamesT tt
 = let	bs		= texBound tt
	vs		= nub $ mapMaybe takeVarOfBound $ Foldable.toList bs
	(_, sub)	= makeNiceVarSub allNiceNames vs
   in	subVV_everywhere sub tt


-- | Beautify the names of locally bound variables.
beautifyLocalNamesT :: Type -> Type
beautifyLocalNamesT tt
 = let	vs		= Set.toList $ collectBindingVarsT tt
	(_, sub)	= makeNiceVarSub allNiceNames vs
   in	subVV_everywhere sub tt
