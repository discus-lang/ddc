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
import Shared.Pretty
import Util
import DDC.Base.SourcePos
import DDC.Var.VarId	
import Data.Char		hiding (isSymbol)
import Shared.Var 		(Var, VarInfo(..), NameSpace(..))
import qualified Data.Map	as Map
import qualified Shared.Var 	as Var


-----
type VarGenM = State VarId


-- | Allocate a fresh variable
newVarN :: NameSpace -> VarGenM Var
newVarN	space
 = do	vid		<- get
	let vid'	= Var.incVarId vid
	put vid'
	
	let var		= (Var.new $ pprStrPlain vid)
			{ Var.varId	= vid
			, Var.nameSpace	= space }
	
	return var


-- | Allocate a fresh variable named after some string
newVarNS ::	NameSpace -> String	-> VarGenM Var
newVarNS	space	     str
 = do	var	<- newVarN space
	return	var { Var.name = (pprStrPlain $ Var.varId var) ++ str }


-- | Allocate a fresh variable named after some string, with info.
newVarNI ::	NameSpace -> [Var.VarInfo]	-> VarGenM Var
newVarNI	space	     info
 = do 	var	<- newVarN space 
	return	var { Var.info = info }
	
	
-- Pretty print the source position of this variable.
prettyPos :: Var	-> String
prettyPos var
 	= fromMaybe "?"
	$ liftM (\(ISourcePos sp) -> pprStrPlain sp)
	$ find (=@= ISourcePos{}) 
	$ Var.info var 

varPos :: Var	-> SourcePos
varPos var
 = do	let Just pos	= liftM (\(ISourcePos sp) -> sp)
			$ find (=@= ISourcePos{})
			$ Var.info var
	pos


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


-- | Check whether the name of this var contains symbols that the
--	C compiler won't like.
varHasSymbols :: Var -> Bool
varHasSymbols var 
 	= not $ null $ filter isSymbol $ Var.name var


-- | Get any sea name on the binding occurrence of this var.
takeSeaNameOfBindingVar :: Var -> Maybe String
takeSeaNameOfBindingVar var
 = let	vBinding : _	= [ v    | Var.IBoundBy v 	<- Var.info var ]
	seaNames	= [ name | Var.ISeaName name	<- Var.info vBinding ]
   in	case seaNames of
		n : _	-> Just n
		_	-> Nothing
		

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


-- | Dummy vars introduced by the compiler won't have SourcePos's
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

