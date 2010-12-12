{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}
module DDC.Core.ToSea.Type
	( toSeaSuperT
	, toSeaT
	, splitOpType
	, stripValues)
where
import DDC.Main.Error
import DDC.Var
import Data.Function
import Util				hiding (mapM)
import Prelude				hiding (mapM)
import qualified Shared.VarPrim		as Var
import qualified DDC.Core.Exp 		as C
import qualified DDC.Type		as T
import qualified DDC.Sea.Exp  		as E
import qualified DDC.Var.PrimId		as Var

stage	= "DDC.Core.ToSea.Type"

-- | Convert an operational type from the core to the equivalent Sea type.
--   For functional types, we convert the types of the arguments as usual.

-- The returned type should always be TFun _ _. If what we have is not a TFun
-- then assume that the type we have is what the NSuper is supposed to return
-- and that the NSuper takes zero parameters.
toSeaSuperT :: T.Type -> E.Type
toSeaSuperT tt
 = case toSeaT' False tt of
	E.TFun at rt	-> E.TFun at rt
	other		-> E.TFun [] other

-- | Convert an operational type from core to equivalent Sea representation type.
--   In the Sea backend, functional values are represented as boxed thunks, so
--   we convert all function types to the type of an anonymous boxed object.
toSeaT :: T.Type -> E.Type
toSeaT tt	= toSeaT' True tt

toSeaT' :: Bool -> T.Type -> E.Type
toSeaT' repr tt
 = let down	= toSeaT' repr
   in case tt of
	T.TForall _ _ t		-> down t
	T.TConstrain t _	-> down t

	T.TApp{}
	 -> let result
		 | Just tx		<- T.takeTData tt
		 = toSeaT_data tx

		 | Just _		<- T.takeTFun tt
		 , tsBits		<- T.flattenTFuns tt
		 , Just tsArgs		<- takeInit tsBits
		 , Just tResult		<- takeLast tsBits
		 = if repr
			then E.tPtrObj
			else E.TFun (map toSeaT tsArgs) (toSeaT tResult)


		 | otherwise
		 = E.tPtrObj

	    in result

	T.TCon{}
	 | Just tx		<- T.takeTData tt
	 -> toSeaT_data tx

	T.TVar{}
	  -> E.tPtrObj

	_ 	-> panic stage
		$ "toSeaT: No match for " ++ show tt ++ "\n"


toSeaT_data tx
	-- The unboxed void type is represented directly.
 	| (v, _, _)			<- tx
 	, VarIdPrim Var.TVoidU	<- varId v
	= E.TVoid

 	 -- We know about unboxed pointers.
	| (v, _, [t])			<- tx
	, VarIdPrim Var.TPtrU	<- varId v
	= E.TPtr (toSeaT t)

	-- The built-in unboxed types are represented directly.
	-- These are types like Int32 and Char.
	| (v, _, [])			<- tx
	, Var.varIsUnboxedTyConData v
	= E.TCon (E.TyConUnboxed v)

	-- An abstract unboxed data type.
	-- The are types like FILE which are defined by the system libraries, and will usually
	-- be be referenced via a pointer.
	-- TODO: To detect these we just check for the '#' characted in the type name, which pretty nasty.
	--       We don't require the arg list to be empty because type constructors like (String# :: % -> *)
	--       have region parameters.
	| (v, _, _)			<- tx
	, elem '#' (varName v)
	= E.TCon (E.TyConAbstract v)

	-- A generic boxed object that the Sea language don't distinguish further.
	-- These are usually ADTs defined in the Disciple source language.
	| otherwise
	= E.tPtrObj


-- | Split a type into its params and return parts.
splitOpType :: T.Type -> ([E.Type], E.Type)
splitOpType to
  = let	opParts		= T.flattenTFuns to
	opParts'@(_:_)	= map toSeaT opParts

	argTypes	= init opParts'
	resultType	= last opParts'
   in 	(argTypes, resultType)


-- | Throw away the type terms in this list of expressions.
stripValues :: [C.Exp] -> [C.Exp]
stripValues args
	= catMaybes
	$ map stripValues' args

stripValues' a
 = case a of
	C.XVar v _
	 |  varNameSpace v /= NameValue
	 -> Nothing

	C.XPrimType _
	 -> Nothing

	_ -> Just a
