{-# OPTIONS -O2 #-}

module Shared.VarUtil
	( VarGenM
	, newVarN
	, newVarNS
	, newVarNI

	, varHasSymbols
	, isSymbol
	, isCtorName
	, isDummy

	, prettyPos
	, prettyPosBound
	, sortForallVars
	
	, deSymString)

where

import qualified Shared.Var as Var
import Shared.Var 			(Var, VarBind, VarInfo(..), NameSpace(..), incVarBind)
import Shared.Pretty

import Util

import Data.Char	hiding (isSymbol)
import qualified Data.Map	as Map
import Control.Monad.State

-----
type VarGenM = State VarBind

newVarN ::	NameSpace -> VarGenM Var
newVarN		space
 = do
 	varBind		<- get
	let varBind'	= Var.incVarBind varBind
	put varBind'
	
	let var		= (Var.new $ pprStrPlain varBind)
			{ Var.bind	= varBind
			, Var.nameSpace	= space }
	
	return var


newVarNS ::	NameSpace -> String	-> VarGenM Var
newVarNS	space	     str
 = do
	var	<- newVarN space
	return	var { Var.name = (pprStrPlain $ Var.bind var) ++ str }


newVarNI ::	NameSpace -> [Var.VarInfo]	-> VarGenM Var
newVarNI	space	     info
 = do
 	var	<- newVarN space 
	return	var { Var.info = info }
	
	
-- Pretty print the source position of this variable.
prettyPos :: Var	-> String
prettyPos var
 	= fromMaybe "?"
	$ liftM (\(ISourcePos sp) -> pprStrPlain sp)
	$ find (=@= ISourcePos{}) 
	$ Var.info var 


-- Pretty print the source position of the bounding occurance of this variable.
prettyPosBound :: Var	-> String
prettyPosBound var
	= fromMaybe "?"
	$ liftM (\(IBoundBy v) -> prettyPos v)
	$ find (=@= IBoundBy{})
	$ Var.info var


-- Sort vars to be quantified into a standard order
sortForallVars :: [Var] -> [Var]
sortForallVars	  vs
 = let
 	tVars	= filter (\v -> Var.nameSpace v == NameType)    vs
	rVars	= filter (\v -> Var.nameSpace v == NameRegion)  vs
	eVars	= filter (\v -> Var.nameSpace v == NameEffect)  vs
	cVars	= filter (\v -> Var.nameSpace v == NameClosure) vs
 in
 	tVars ++ rVars ++ eVars ++ cVars


-- Check whether the name of this var contains symbols that the
--	C compiler won't like.
varHasSymbols :: Var -> Bool
varHasSymbols var 
 	= not $ null $ filter isSymbol $ Var.name var


-- | Check if this char is a symbol
--	everything except alpha, numeric, and '_' is a symbol.
isSymbol :: Char -> Bool
isSymbol c
	| isAlphaNum c 	= False
	| c == '_'	= False
	| otherwise	= True	


-- | Check if this var as the name of a constructor.
--	Constructors start with an uppercase latter.
isCtorName :: Var -> Bool
isCtorName var 
	= isUpper n
	where (n:_)	= Var.name var


-- Dummy vars introduced by the compiler won't have SourcePos's
isDummy	   var	
	= not $ any (=@= ISourcePos{}) 		 
	$ Var.info var	
	

-- | Rewrite symbolic chars in a string 
deSymString :: String -> String
deSymString s
 = catMap (\c -> fromMaybe [c] (Map.lookup c deSymSub)) s

deSymSub
	= Map.fromList
	[ ('!', "Bg")
	, ('@', "At")
	, ('#', "Hs")
	, ('$', "Dl")
	, ('%', "Pc")
	, ('^', "Ht")
	, ('&', "An")
	, ('*', "St")
	, ('~', "Tl")
	, ('-', "Ms")
	, ('+', "Ps")
	, ('=', "Eq")
	, ('|', "Pp")
	, ('\\', "Bs")
	, ('/', "Fs")
	, (':', "Cl")
	, ('.', "Dt")
	, ('?', "Qm")
	, ('<', "Lt")
	, ('>', "Gt")
	, ('[', "Br")
	, (']', "Kt")
	, ('\'', "Pm")
	, ('`', "Bt") ]

