{-# OPTIONS -O2 #-}

module Shared.VarUtil
	( VarGenM
	, newVarN
	, newVarNS
	, newVarNI
	, varHasSymbols
	, takeSeaNameOfBindingVar
	, isSymbol
	, isCtorName
	, isDummy
	, varPos
	, prettyPos
	, prettyPosBound
	, sortForallVars
	, deSymString)
where
import Util
import DDC.Base.SourcePos
import DDC.Var
import DDC.Main.Pretty
import Data.Char		hiding (isSymbol)
import qualified Data.Map	as Map


-----
type VarGenM = State VarId


-- | Allocate a fresh variable
newVarN :: NameSpace -> VarGenM Var
newVarN	space
 = do	vid		<- get
	let vid'	= incVarId vid
	put vid'

	let var		= (varWithName $ pprStrPlain vid)
			{ varId		= vid
			, varNameSpace	= space }

	return var


-- | Allocate a fresh variable named after some string
newVarNS ::	NameSpace -> String	-> VarGenM Var
newVarNS	space	     str
 = do	var	<- newVarN space
	return	var { varName = (pprStrPlain $ varId var) ++ str }


-- | Allocate a fresh variable named after some string, with info.
newVarNI ::	NameSpace -> [VarInfo]	-> VarGenM Var
newVarNI	space	     info
 = do 	var	<- newVarN space
	return	var { varInfo = info }


-- Pretty print the source position of this variable.
prettyPos :: Var	-> String
prettyPos var
 	= fromMaybe "?"
	$ liftM (\(ISourcePos sp) -> pprStrPlain sp)
	$ find isISourcePos
	$ varInfo var

varPos :: Var	-> SourcePos
varPos var
 = do	let pos	= liftM (\(ISourcePos sp) -> sp)
			$ find isISourcePos
			$ varInfo var
	case pos of
	  Just p	-> p
	  Nothing	-> SourcePos ("?", 0, 0)


-- Pretty print the source position of the bounding occurance of this variable.
prettyPosBound :: Var	-> String
prettyPosBound var
	= fromMaybe "?"
	$ liftM (\(IBoundBy v) -> prettyPos v)
	$ find isIBoundBy
	$ varInfo var


-- Sort vars to be quantified into a standard order
sortForallVars :: [Var] -> [Var]
sortForallVars	  vs
 = let
 	tVars	= filter (\v -> varNameSpace v == NameType)    vs
	rVars	= filter (\v -> varNameSpace v == NameRegion)  vs
	eVars	= filter (\v -> varNameSpace v == NameEffect)  vs
	cVars	= filter (\v -> varNameSpace v == NameClosure) vs
 in
 	tVars ++ rVars ++ eVars ++ cVars


-- | Check whether the name of this var contains symbols that the
--	C compiler won't like.
varHasSymbols :: Var -> Bool
varHasSymbols var
 	= not $ null $ filter isSymbol $ varName var


-- | Get any sea name on the binding occurrence of this var.
takeSeaNameOfBindingVar :: Var -> Maybe String
takeSeaNameOfBindingVar var
	| vBinding : _	<- [ v    | IBoundBy v 		<- varInfo var ]
	, seaNames	<- [ name | ISeaName name	<- varInfo vBinding ]
	, n : _		<- seaNames
	= Just n

	| otherwise
	= Nothing

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
	where (n:_)	= varName var


-- | Dummy vars introduced by the compiler won't have SourcePos's
isDummy	   var
	= not $ any isISourcePos
	$ varInfo var


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

