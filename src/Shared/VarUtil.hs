
module Shared.VarUtil
	( VarGenM
	, newVarN
	, newVarNS
	, newVarNI

	, isSymbol
	, isCtorName
	, isDummy

	, prettyPos
	, prettyPosBound
	, sortForallVars
	
	, deSymString)

where

import Data.Char	hiding (isSymbol)

import qualified Shared.Var as Var
import Shared.Var 
	(Var, VarBind, VarInfo(..), NameSpace(..), incVarBind)

import Shared.Pretty
import Util

import qualified Data.Map	as Map

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
	
	

{-
cookName ::	Var	-> String
cookName	var
	-- Wrap symbols in parenthesis ala Haskell.
	| isSymbol var	= "("  ++ Var.name var ++ ")"
	
	-- Prefix dummy variables with underscore to guarantee that they don't clash with user variables.
	| isDummy  var	= "_"  ++ Var.name var
-}
	
prettyPos :: Var	-> String
prettyPos var
 	= fromMaybe "?"
	$ liftM (\(ISourcePos sp) -> pprStrPlain sp)
	$ find (=@= ISourcePos{}) 
	$ Var.info var 

prettyPosBound :: Var	-> String
prettyPosBound var
	= fromMaybe "?"
	$ liftM (\(IBoundBy v) -> prettyPos v)
	$ find (=@= IBoundBy{})
	$ Var.info var


-----
sortForallVars :: [Var] -> [Var]
sortForallVars	  vs
 = let
 	tVars	= filter (\v -> Var.nameSpace v == NameType)    vs
	rVars	= filter (\v -> Var.nameSpace v == NameRegion)  vs
	eVars	= filter (\v -> Var.nameSpace v == NameEffect)  vs
	cVars	= filter (\v -> Var.nameSpace v == NameClosure) vs
 in
 	tVars ++ rVars ++ eVars ++ cVars

-----
isSymbol   var 
	= (not $ isAlpha n) && (n /= '_')
	where (n:_)	= Var.name var

isCtorName var 
	= isUpper n
	where (n:_)	= Var.name var

isDummy	   var	
	= not $ any (=@= ISourcePos{}) 		-- Dummy vars introduced by the compiler won't have SourcePos's 
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

