
module DDC.Type.Collect.Textual
	( TexBound (..) )
where
import DDC.Type.Compounds
import DDC.Type.Exp
import Control.Monad
import Data.Sequence		(Seq)
import qualified Data.Sequence	as Seq
import qualified Data.Map	as Map

class TexBound a where
 -- | Collect each occurrence of a variable in some thing, following the
--    textual ordering when pretty printed, that is a left-right walk
--    over the AST. Vars in both binding and bound positions are returned.
 texBound :: a -> Seq Bound

instance TexBound a => TexBound [a] where
 texBound xx	= join $ Seq.fromList $ map texBound xx

instance (TexBound a, TexBound b) => TexBound (a, b) where
 texBound (x, y) = texBound x Seq.>< texBound y

instance TexBound Bound where
 texBound u	= Seq.singleton u


instance TexBound Bind where
 texBound bb
  = case bb of
	BVar v		-> Seq.singleton $ UVar v
	BMore v t	-> Seq.singleton $ UMore v t


instance TexBound Kind where
 texBound kk
  = case kk of
	KNil		-> Seq.empty
	KCon _ _	-> Seq.empty
	KFun k1 k2	-> texBound k1 Seq.>< texBound k2
	KApp k  t	-> texBound k  Seq.>< texBound t
	KSum ks		-> texBound ks

	
instance TexBound Type where
 texBound tt
  = case tt of
	TNil		-> Seq.empty
	TVar _ u	-> texBound u
	TCon{}		-> Seq.empty
	TSum k ts	-> texBound k  Seq.>< texBound ts

	-- The args for the function constructor are displayed
	-- in a diff order relative to the type applications.
	TApp t1 t2
	 | Just (t1, t2, eff, clo) <- takeTFun tt
	 ->        texBound t1  
	    Seq.>< texBound eff
	    Seq.>< texBound clo
	    Seq.>< texBound t2
	
	 | otherwise
	 -> texBound t1 Seq.>< texBound t2
	
	-- Make vars from the body take priority over the quantified var.
	TForall b _ t	-> texBound t  Seq.>< texBound b

	TConstrain t cs -> texBound t  Seq.>< texBound cs

	TError{} 	-> Seq.empty


instance TexBound Constraints where
 texBound (Constraints crsEq' crsMore' crsOther')
  	=      (texBound $ Map.toList crsEq')
	Seq.>< (texBound $ Map.toList crsMore')
	Seq.>< (texBound crsOther')
	

instance TexBound Fetter where
 texBound ff 
  = case ff of
	FConstraint _ ts	-> texBound ts
	FWhere t1 t2		-> texBound t1 Seq.>< texBound t2
	FMore  t1 t2		-> texBound t1 Seq.>< texBound t2
	FProj{}			-> Seq.empty
	