
-- | Tool to generate tree-walking boilerplate code for our expression data types.
--	It's pretty boring to write these ourselves.
--	and marginally less boring to write the boiler-plate generator
--	It might be better to move this to template Haskell, but it might be easier
--	to leave it like this for future bootstrapping.
import System.Environment
import Language.Haskell.Syntax
import Language.Haskell.Parser
import Language.Haskell.Pretty
import Util


-- Helpers -----------------------------------------------------------------------------------------
ident x		= HsIdent x
uIdent x	= UnQual (HsIdent x)

var x		= HsVar (UnQual (HsIdent x))

tyVar m		= HsTyVar (HsIdent m)
tyCon x		= HsTyCon (UnQual (HsIdent x))
tyApp x1 x2	= HsTyApp x1 x2

pVar x		= HsPVar (HsIdent x)

makeApp :: [HsExp] 	-> HsExp
makeApp [x]		= x
makeApp	(x1:x2:xs)	= makeApp ((HsApp x1 x2) : xs)
	

----------------------------------------------------------------------------------------------------
main 
 = do	[fileIn, fileStub, fileOut] <- getArgs
	expandFile fileIn fileStub fileOut
	
-- make boiler plate for this file
expandFile :: FilePath -> FilePath -> FilePath -> IO ()
expandFile fileIn fileStub fileOut
 = do	
	-- read in the source file containg the data defs
 	srcIn		<- readFile fileIn

	-- parse the soruce
	let ParseOk mod	= parseModule srcIn

	-- expand out boiler plate
	let decls'	= expandModule mod

	-- make the output file from the stub and the generated code
	srcStub		<- readFile fileStub
	
	writeFile fileOut 
		$  srcStub ++ "\n\n" 
		++ catInt "\n" (map prettyPrint decls')

	return ()


-- make boilerplate for this module
expandModule :: HsModule -> [HsDecl]
expandModule mod
 = case mod of
 	HsModule loc m@(Module name) exports imports decls
	 -> catMap expandDecl decls

-- make boilerplate for a data type
expandDecl :: HsDecl -> [HsDecl]
expandDecl dd
 = case dd of
 	HsDataDecl loc context name args decls derive
	 -> maybeToList $ expandTransM dd
	 
	_	-> []


-- TransM ------------------------------------------------------------------------------------------
expandTransM :: HsDecl -> Maybe HsDecl
expandTransM (HsDataDecl src context (HsIdent con1) args decls derive)
 = let	sTrans	= HsFunBind
 		[ HsMatch src
			(ident $ "transZM")
			[ pVar "table", pVar "xx"]
			(HsUnGuardedRhs 
				$ makeApp 
					[ var "transMe" 
					, HsParen (HsApp (var $ "trans" ++ con1) (var "table"))
					, HsParen (HsApp (var $ "trans" ++ con1 ++ "_enter") (var "table"))
					, HsParen (HsApp (var $ "trans" ++ con1 ++ "_leave") (var "table"))
					, var "table"
					, var "xx"
					])
			[]
		]
 
 	sFollow	= HsFunBind
 		[ HsMatch src
			(ident "followZM")
			[ pVar "table", pVar "xx"]
			(HsUnGuardedRhs $ HsCase (var "xx") (map transM_decl decls))
			[]
		]

	tCon name
		| []	<- args		= tyCon con1
		| otherwise		= tyApp (tyCon con1) (tyVar name)

   in	Just	
	$ HsInstDecl src
		[ (uIdent "Monad", [tyVar "m"]) ]
		(uIdent "TransM")
		[tyVar "m", tyVar "n1", tyVar "n2"
			, tCon "n1"
			, tCon "n2" ]
		[ sTrans, sFollow]
		
expandTransM _
	= Nothing


-- | Generate transform code for a constructor decl.
transM_decl :: HsConDecl -> HsAlt
transM_decl decl
 = case decl of
	HsConDecl src name types
	 -> transM_decl' src name types
		
	HsRecDecl src name fsTypes
	 -> transM_decl' src name (map snd fsTypes)

	
transM_decl' src name types
 = let	tmps		= map (\ix -> ("x" ++ show ix)) [0.. length types -1]
	tmps'		= map (\v -> v ++ "'") tmps

	ssFollow	= zipWith (transM_stmt src types) [0..] types
	sRet		= HsQualifier $
				HsApp 	(var "return")
					(HsParen $ makeApp (HsVar (UnQual name) : map var tmps'))
	
   in
	HsAlt src 
		(HsPApp (UnQual name) (map pVar tmps))
		(HsUnGuardedAlt
			$ HsDo 	(ssFollow ++ [sRet]))
		[]




transM_stmt 
	:: SrcLoc 
	-> [HsBangType]		-- all type args of the ctor
	-> Int 			-- index of this ctor
	-> HsBangType 
	-> HsStmt

transM_stmt src tArgs ix ta@(HsUnBangedTy tt)

	-- if the first type arg is a variable, treat it as the
	--	annotation type and transform it with transN
	| Just t1@(HsUnBangedTy (HsTyVar _))	
			<- takeHead tArgs
	, ta == t1
	= HsGenerator src 
		(pVar 	$ "x" ++ show ix ++ "'") 
		(makeApp [var "transN", var "table", var $ "x" ++ show ix])

	-- otherwise decend into it 
	| otherwise
	= HsGenerator src 
		(pVar 	$ "x" ++ show ix ++ "'") 
		(makeApp [var "transZM", var "table", var $ "x" ++ show ix])


