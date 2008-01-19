-----
-- Source.Parser
--
-- Summary:
--	Do the minimal amount of work in the parser. 
--	Try not to do any desugaring here.
--	Source parse tree should preserve structure of program as entered by user.
--	This way we'll get better error messages during type checking.
--
{
module Source.Parser 
	(parse) 

where

-----
import Data.Char

-----
import Util

-----
import qualified Shared.Var 	as Var
import Shared.Base		(SourcePos(..))
import Shared.Var 		(NameSpace(..), Module(..))
import Shared.VarPrim

import Shared.Error
import Source.Error


import qualified Source.Token 	as K
import Source.Token 		(TokenP(..), Token, token)
import Source.Exp
import Source.Util
import Type.Util		(pure, empty, takeKindOfType)

import Debug.Trace

stage 	= "Source.Parser"

}

%name 		parse
%tokentype	{ TokenP }

%token
	'pragma'	{ TokenP { token = K.Pragma	} }

	'foreign'	{ TokenP { token = K.Foreign	} }
	'import'	{ TokenP { token = K.Import	} }
--	'export'	{ TokenP { token = K.Export	} }

	'module'	{ TokenP { token = K.Module	} }
	'elaborate'	{ TokenP { token = K.Elaborate	} }
	'const'		{ TokenP { token = K.Const	} }
	'mutable'	{ TokenP { token = K.Mutable	} }
	'extern'	{ TokenP { token = K.Extern	} }
--	'ccall'		{ TokenP { token = K.CCall	} }
	
--	'type'		{ TokenP { token = K.Type	} }
	'data'		{ TokenP { token = K.Data	} }
	'region'	{ TokenP { token = K.Region	} }
	'effect'	{ TokenP { token = K.Effect     } }

	'class'		{ TokenP { token = K.Class	} }
	'instance'	{ TokenP { token = K.Instance   } }
	'project'	{ TokenP { token = K.Project	} }

	'infixr'	{ TokenP { token = K.InfixR	} }
	'infixl'	{ TokenP { token = K.InfixL	} }
	'infix'		{ TokenP { token = K.Infix	} }

	'let'		{ TokenP { token = K.Let	} }
	'in'		{ TokenP { token = K.In		} }
	'where'		{ TokenP { token = K.Where	} }

	'case'		{ TokenP { token = K.Case	} }
	'of'		{ TokenP { token = K.Of		} }
	'match'		{ TokenP { token = K.Match	} }

	'if'		{ TokenP { token = K.If		} }
	'then'		{ TokenP { token = K.Then	} }
	'else'		{ TokenP { token = K.Else	} }
	
	'throw'		{ TokenP { token = K.Throw	} }
	'try'		{ TokenP { token = K.Try	} }
	'catch'		{ TokenP { token = K.Catch	} }
	'with'		{ TokenP { token = K.With	} }
	
	'do'		{ TokenP { token = K.Do		} }
	'while'		{ TokenP { token = K.While	} }
	'when'		{ TokenP { token = K.When	} }
	'unless'	{ TokenP { token = K.Unless	} }
	'break'		{ TokenP { token = K.Break	} }
	
	'forall'	{ TokenP { token = K.Forall	} }
		
	'::'		{ TokenP { token = K.HasType	   } }
	
--	'<:'		{ TokenP { token = K.IsSubtypeOf   } }
--	'<*'		{ TokenP { token = K.IsSubtypeOfQuant } }

	':$'		{ TokenP { token = K.HasOpType	   } }
	':-'		{ TokenP { token = K.HasConstraint } }

	'->'		{ TokenP { token = K.RightArrow } }
	'()'		{ TokenP { token = K.Unit	} }
	
	'|-'		{ TokenP { token = K.GuardCase } }
	',-'		{ TokenP { token = K.GuardCaseC } }

	'|#'		{ TokenP { token = K.GuardUnboxed } }
	',#'		{ TokenP { token = K.GuardUnboxedC } }

	'\\='		{ TokenP { token = K.GuardDefault } }
	
	'{'		{ TokenP { token = K.CBra	} }
	'}'		{ TokenP { token = K.CKet	} }
	
	'('		{ TokenP { token = K.RBra	} }
	')'		{ TokenP { token = K.RKet	} }

	'['		{ TokenP { token = K.SBra	} }
	']'		{ TokenP { token = K.SKet	} }

	'<'		{ TokenP { token = K.ABra	} }
	'>'		{ TokenP { token = K.AKet	} }
	
	'\\'		{ TokenP { token = K.BackSlash	} }
	'`'		{ TokenP { token = K.BackTick	} }
	'='		{ TokenP { token = K.Equals	} }
	','		{ TokenP { token = K.Comma	} }	
	':'		{ TokenP { token = K.Colon	} }
	';'		{ TokenP { token = K.SemiColon	} }
	'|'		{ TokenP { token = K.Bar	} }
	'.'		{ TokenP { token = K.Dot	} }
	'&'		{ TokenP { token = K.And	} }

	'#'		{ TokenP { token = K.Hash	} }
	'*'		{ TokenP { token = K.Star	} }
	'-'		{ TokenP { token = K.Dash	} }
	'+'		{ TokenP { token = K.Plus	} }
	'%'		{ TokenP { token = K.Percent	} }
	'@'		{ TokenP { token = K.At		} }
	'!'		{ TokenP { token = K.Bang	} }	
	'/'		{ TokenP { token = K.ForwardSlash } }
	'$'		{ TokenP { token = K.Dollar	} }
	'_'		{ TokenP { token = K.Underscore	} }
	'^'		{ TokenP { token = K.Hat	} }
	'..'		{ TokenP { token = K.DotDot	} }
	'<-'		{ TokenP { token = K.LeftArrow	} }
	'<@-'		{ TokenP { token = K.LeftArrowLazy } }
		
	VAR		{ TokenP { token = K.Var    _	} }
	CON		{ TokenP { token = K.Con    _   } }
	SYM		{ TokenP { token = K.Symbol _	} }
	MODULENAME	{ TokenP { token = K.ModuleName _ } }
	
	INT		{ TokenP { token = K.CInt    _	} }
	CHAR		{ TokenP { token = K.CChar   _  } }
	FLOAT		{ TokenP { token = K.CFloat  _	} }
	STRING		{ TokenP { token = K.CString _	} }

%%


------------------------------------------------------------------------------------------------------------------------------------ 

--------------------
-- Top level
--------------------

-----
tops		
	:: { [Top] }
	:  {- empty -}				{ []					}
	|  top tops				{ $1 ++ $2				}

-----
top		
	:: { [Top] }
	: 'pragma' expApps ';'				{ [PPragma $2]					}
	| 'module' module ';'				{ [PModule $2]					}
		 
	| 'import' 'extern'  externSig			{ [$3]						}
	| 'import' 'extern' '{' externSigs '}' 		{ $4						}
		
	| 'import' module        ';'			{ [PImportModule   [$2]]			}
	| 'import' '{' module_semi '}' 			{ [PImportModule   $3]				}

	| 'foreign' foreign				{ [PForeign $2]					}


	| infixMode INT symbolList ';'			{ [PInfix    $1 (getCIntValue $2) $3]		}
	| dataDef ';'					{ [$1]						}

	| 'effect' pCon '::' kind ';'			{ [PEffect  (vNameE $2) $4]			}
	| 'region' pVar ';'				{ [PRegion  (vNameR $2)]			}

	-- class defs
	| 'class' pCon '::' kind ';' 
	{ [PClass   (vNameW $2) $4] }

	| 'class' pCon pVar 'where' '{' typeSig_Ss '}' 	
	{ [PClassDict (vNameW $2) [vNameT $3] [] $6 ] }

	| 'instance' pCon typeZ_space 'where' '{' binds '}'
	{ [PClassInst (vNameW $2) $3 [] $6 ] }

	-- projection defs
	| 'project' typeA 'where' '{' stmtSigBinds '}'
	{ [PProjDict $2 $5]		}

	-- type sigs
	| pVar '::' type ';'	
	{ [PType (spTP $2) (vNameV $1) $3]						}
		
	-- statements
	| expApps '=' exp ';'				
	{ [PStmt (SBindPats (spTP $2) (checkVar $2 $ head $1) (tail $1) $3)]		}


-----
-- Foreign
--
foreign		
	:: { Foreign }
	: 'import' foreignExtern			{ OImport $2				}
		
foreignExtern	
	:: { Foreign }
	: 'extern' mString pVar '::' type ';'		 { OExtern $2 $3 $5 Nothing	}
	| 'extern' mString pVar '::' type ':$' typeN ';' { OExtern $2 $3 $5 (Just $7) 	}


mString		
	:: { Maybe String }
	: {- empty -}				{ Nothing					}
	| STRING				{ Just ((\(K.CString s) -> s) (token $1))	}

-----
-- Modules
--
module		
	:: { Module }

	-- module names look remarkably like qualified constructors...
	: qCon					
	{ ModuleAbsolute
	   $	(case Var.nameModule $1 of 
			ModuleAbsolute strs	-> strs
			_			-> [])
						
		++ [Var.name $1]
	}

module_parts
	:: { [String] }
	: CON 					{ let K.Con str = token $1 in [str]	}
	| CON '.' module_parts			{ let K.Con str = token $1 in str : $3	}

module_semi
	:: { [Module] }
	: {- empty -}				{ [] 					}
	| module				{ [$1] 					}
	| module ';' module_semi		{ $1 : $3 				}	

infixMode	
	:: { InfixMode }	
	: 'infixl'				{ InfixLeft				}
	| 'infixr'				{ InfixRight				}		
	| 'infix'				{ InfixNone				}

symbolList
	:: { [Var] }
	: symbol				{ [$1]					}
	| symbol ',' symbolList			{ $1 : $3				}

externSig
	:: { Top }
	: pVar '::' type ':$' typeN ';'		{ PImportExtern $1 $3 (Just $5)		}

externSigs
	:: { [Top] }
	: externSig				{ [$1]					}
	| externSig externSigs			{ $1 : $2				}

typeSig
	:: { ([Var], Type) }
	: pVars_comma '::' type ';'		{ ($1, $3)				}
			
typeSig_Ss
	:: { [([Var], Type)] }
	: typeSig				{ [$1]					}
	| typeSig typeSig_Ss			{ $1 : $2				}
			

-----
-- ClassCstrs
--	Type class constraints
--
{-
classCstrs	
	:: { [(Var, [Var])] }				
	: {- empty -}				{ [] }
	| ':-' classCstrs2			{ $2 }
		
classCstrs2
	:: { [(Var, [Var])] }
	: classCstr				{ [$1]					}
	| classCstr ',' classCstrs2		{ $1 : $3				}
	
classCstr
	:: { (Var, [Var]) }
	:  pCon qVars				{ ($1, $2)				}
-}
		
-----
-- InstCstrs
--	Instance constraints
--
{-
instCstrs
	:: { [(Var, [Type])] }
	: {- empty -}				{ []					}
	| ':-' instCstrs2			{ $2					}
		
instCstrs2
	:: { [(Var, [Type])] }
	: instCstr				{ [$1]					}
	| instCstr ',' instCstrs2		{ $1 : $3				}
		
instCstr
	:: { (Var, [Type]) }
	: CON typeZ_Ssp				{ (toVarS $1, $2)			}
-}

--------------------
-- Expressions
--------------------

-----
exp	:: { Exp }
	: 'try' expE tryCatch tryWith		{ XTry		(spTP $1) $2 $3 $4	}
	| expE					{ $1}
	
tryCatch
	:: { [Alt] }
	: {- empty -}				{ []					}
	| 'catch' '{' caseAlts '}' tryCatch	{ $3 ++ $5				}

tryWith	
	:: { Maybe Exp }
	: {- empty -}				{ Nothing				}
	| 'with'  expE				{ Just $2				}
		
expE	:: { Exp }
	: expE2					{ $1						}
	| '\\' 	   expApps '->'  expE		{ XLambdaPats   (spTP $1) $2 $4			}		
	| '\\' '.' pVar expAppZs		{ XLambdaProj   (spTP $1) (JField  $3) $4	}

	| 'if'     exp 'then' exp 'else' expE	{ XIfThenElse 	(spTP $1) $2 $4 $6	}

	| 'when'   expApp expE			{ XWhen		(spTP $1) $2 $3		}
	| 'unless' expApp expE			{ XUnless	(spTP $1) $2 $3		}
	| 'while'  expApp expE			{ XWhile	(spTP $1) $2 $3		}

expE2	:: { Exp }
	: expInfix				{ $1					}
	| 'let' '{' stmts '}' 'in' expInfix	{ XLet		(spTP $1) $3 $6		}
	| 'throw'  expInfix			{ XThrow	(spTP $1) $2		}

expInfix
	:: { Exp }
	: expApps				{ case $1 of
								[s]	-> s
								(s:_)	-> XDefix (spX s) $1	}

expApps
	:: { [Exp] }
	: expApp				{ [$1]					}
	| expApp symbol expApps			{ $1 : XOp (spX $1) (vNameV $2) : $3	}
	| expApp expApps			{ $1 : $2				}

expAppZs
	:: { [Exp] }
	: {- empty -}				{ []					}
	| expApp expAppZs			{ $1 : $2				}
		
expApp
	:: { Exp }
	: expZ					{ $1					}

	| '`' qVar '`'				{ XOp  	(spV $2) (vNameV $2)		}
	| '`' qCon '`'				{ XOp	(spV $2) (vNameV $2)		}			
		
	| const					{ $1					}
	| tuple					{ $1					}
	| list					{ $1					}
	

expZ
	:: { Exp }
	: expA					{ $1					}

	-- do
	| 'do'  '{' stmts '}'			{ XDo           (spTP $1) $3		}

	-- case / match
	| 'case' exp 'of' '{' caseAlts '}'	{ XCase 	(spTP $1) $2 $5		} 
		
	| 'match' '{' matchAlts '}'		{ XMatch	(spTP $1) $3		}

	-- lambda sugar
	| '\\' 'case' '{' caseAlts '}'		{ XLambdaCase	(spTP $1) $4		}

	| qCon					{ XVar		(spV $1) (vNameV $1)	}


expA	
	:: { Exp }				

	: '(' exp ')'				{ $2					}

	-- field projections
	| expA '.' pVar				{ XProj  (spTP $2) $1 (JField  $3)	}
	| expA '#' pVar				{ XProj  (spTP $2) $1 (JFieldR $3)	}

	| pVar '&' '{' typeN '}'		{ XProjT (spTP $2) $4 (JField  $1)	}

	-- index projection
	| expA '.' '(' exp ')'			{ XProj  (spTP $2) $1 (JIndex  $4) 	}
	| expA '#' '(' exp ')'			{ XProj  (spTP $2) $1 (JIndexR $4)	}

	-- object syntax
	| '^' pVar				{ XObjVar   (spTP $1) (vNameV $2)	}
	| '_' pVar				{ XObjField (spTP $1) (vNameF $2)	}

	| 'break'				{ XBreak  (spTP $1)			}
	| '()'					{ XVar	  (spTP $1) (makeVar "Unit" $1)	}

	| qVar 					{ XVar 	  (spV $1) (vNameV $1)		}

		

-- literal constants
const
	:: { Exp }
	:  INT   	constU			{ makeConst    	$2 $1			}
	|  CHAR		constU			{ makeConst 	$2 $1			}
	|  FLOAT	constU			{ makeConst 	$2 $1 			}
	|  STRING	constU			{ makeConst	$2 $1			}


constU
	:: { Bool }
	: {- empty -}				{ False }
	| '#'					{ True }


-----
stmt
	:: {  Stmt }
	:  exp          ';'			{ SBind (spX $1) Nothing $1		}
	|  bind					{ $1					}
	|  pVar '::' type ';'			{ SSig (spTP $2) (vNameV $1) $3		}

stmts
	:: { [Stmt] }
	:  stmt					{ [$1] 					}
	|  stmt stmts				{ $1 : $2 				}

bind
	:: { Stmt }
	:  expApps '=' exp ';'			{ SBindPats (spTP $2) (checkVar $2 $ head $1) (tail $1) $3	}

binds
	:: { [Stmt] }
	:  bind					{ [$1] }
	|  bind binds				{ $1 : $2 }

stmtSigBinds
	:: { [Stmt] }
	: stmtSigBind				{ [$1] }
	| stmtSigBind stmtSigBinds		{ $1 : $2 }
		
stmtSigBind
 	:: { Stmt }
	:  bind					{ $1					}
	|  pVar '::' type ';'			{ SSig (spTP $2) (vNameV $1) ($3)	}


-----
caseAlts
	:: { [Alt] }
	:  caseAlt				{ [$1]					}
	|  caseAlt caseAlts			{ $1 : $2				}

caseAlt
	:: { Alt }				
	:  pat      '->' exp ';'		{ APat $1 $3				}
	|  '_'      '->' exp ';'		{ ADefault $3				}

-----
matchAlts
	:: { [Alt] }
	:  matchAlt				{ [$1]					}
	|  matchAlt matchAlts			{ $1 : $2				}
		
matchAlt
	:: { Alt }
--	:  '|' exp '=' exp ';'			{ AGuard (spTP $1) $2 $4		}

	:  guards '=' exp ';'			{ AAlt $1 $3				}
	|  '\\=' exp	';'			{ AAlt [] $2				}
		
guards	:: { [Guard] }
	: guard1				{ [$1]					}
	| guard1 guard2s			{ $1 : $2				}

guard1	:: { Guard }
	: '|'  pat '<-' exp			{ GExp   $2 $4				}
	| '|'  exp				{ GBool  $2				}
		
	| '|-' pat				{ GCase  $2				}
	| '|#' exp				{ GBoolU $2				}



guard2s	:: { [Guard] }
	:  guard2				{ [$1]					}
	|  guard2 guard2s			{ $1 : $2				}

guard2	:: { Guard }
	: ',' pat '<-' exp			{ GExp	$2 $4				}
	| ',' exp				{ GBool	$2				}

	| ',-' pat				{ GCase $2				}
	| ',#' exp				{ GBoolU $2				}

		
pat	:: { Pat }
	: expInfix				{ WExp    $1				}

	| pCon '{' '}'				{ WConLabel $1 []			}
	| pCon '{' labelPat_Cs '}'		{ WConLabel $1 $3			}


labelPat_Cs
	:: { [(Label, Pat)] }
	: labelPat				{ [$1]					}
	| labelPat ',' labelPat_Cs		{ $1 : $3				}
		
labelPat
	:: { (Label, Pat) }
	: '.' pVar '=' pVar			{ (LVar $2, WVar $4)				}
	| '.' INT  '=' pVar			{ (LIndex (getCIntValue $2), WVar $4)	}
		

-----------------------
-- Constructor Sugar
-----------------------

-----
tuple	:: { Exp }			
	: '(' exp ',' tupleExps ')'		{ XTuple (spTP $1) ($2:$4)		}
		
tupleExps
	:: { [Exp] }
	:  exp					{ [$1] 					}
	|  exp ',' tupleExps			{ $1 : $3				}

-----
list	:: { Exp }
	: '[' ']'				{ XList (spTP $1) []				}
	| '[' listExps ']'			{ XList (spTP $1) $2				}
	| '[' exp '..' exp ']'			{ XListRange  (spTP $1) False $2 (Just $4)	}
	| '[' exp '..' ']'			{ XListRange  (spTP $1) True  $2 Nothing	}

	| '[' '@' exp '..' exp ']'		{ XListRange  (spTP $1) True  $3 (Just $5)	}
	| '[' '@' exp '..' ']'			{ XListRange  (spTP $1) True  $3 Nothing	}

	| '[' exp '|' lcQual_Cs ']'		{ XListComp   (spTP $1) $2 $4			}

listExps
	:: { [Exp] }
	: exp					{ [$1]					}		
	| exp ',' listExps			{ $1 : $3				}


lcQual_Cs
	:: { [LCQual] }
	: lcQual				{ [$1]					}
	| lcQual ',' lcQual_Cs			{ $1 : $3				}

lcQual	:: { LCQual }
	: exp '<-'  exp				{ LCGen False $1 $3			}
	| exp '<@-' exp				{ LCGen True  $1 $3			}
	| exp					{ LCExp $1				}
		


---------------------
-- Data Definitions
---------------------

dataDef	:: { Top }
	: 'data' pCon pVars_space_empty				
	{ PData $2 (map (vNameDefaultN NameType) $3) []	}

	| 'data' pCon pVars_space_empty '=' dataConss		
	{ PData $2 (map (vNameDefaultN NameType) $3) $5	}

	| 'data' CON  '#' pVars_space_empty
	{ PData (toVarHash NameType $2) (map (vNameDefaultN NameType) $4) []	}
		
	| 'data' CON  '#' pVars_space_empty 'foreign' STRING 	
	{ 
		let	K.CString name	= token $6
			var		= (toVarHash NameType $2) { Var.info = [Var.ISeaName name] }
	  	in	PData var (map (vNameDefaultN NameType) $4) [] 
	}
		
dataConss
	:: { [(Var, [DataField Exp Type])] }
	: dataCons					{ [$1]					}
	| dataCons '|' dataConss			{ $1 : $3				}

dataCons
	:: { (Var, [DataField Exp Type]) }
	: pCon						{ ($1, [])				}
	| pCon dataType_Ss				{ ($1, $2)				}
	| pCon '{' dataField_Ss '}'			{ ($1, $3)				}
		
dataField
	:: { DataField Exp Type }
	: type ';'					{ DataField 
								{ dPrimary	= True
								, dLabel	= Nothing
								, dType		= $1
								, dInit		= Nothing } }
								

	| pVar '::' type ';'				{ DataField 
								{ dPrimary	= True	
								, dLabel 	= Just (vNameF $1)
								, dType		= $3
								, dInit		= Nothing } }
								
	| '.' pVar '::' type dataInit ';'		{ DataField 
								{ dPrimary	= False
								, dLabel	= Just (vNameF $2)
								, dType		= $4
								, dInit		= $5 } 		}
dataInit	
	:: { Maybe Exp }
	: 						{ Nothing 				}
	| '=' exp					{ Just $2 				}
		

dataField_Ss
	:: { [DataField Exp Type] }
	: dataField					{ [$1]					}
	| dataField dataField_Ss			{ $1 : $2				}

dataType
 	:: { DataField Exp Type}
	: typeZ						{ DataField
								{ dPrimary	= True
								, dLabel	= Nothing
								, dType		= $1
								, dInit		= Nothing } }
								
dataType_Ss
	:: { [DataField Exp Type] }
	: dataType					{ [$1]					}
	| dataType dataType_Ss				{ $1 : $2				}


--------------------------
-- Kind Expressions
-------------------------

kind	:: { Kind }
	: kindA						{ $1					}
	| kindA '->' kind				{ KFun $1 $3				}

kindA	:: { Kind }
	: '*'						{ KData					}
	| '%'						{ KRegion				}
	| '!'						{ KEffect				}
	| '+'						{ KFetter				}

								

-----------------------
-- Type Expressions
-----------------------
type	:: { Type }
	: typeQuant					{ $1					}
	| 'elaborate' type				{ TElaborate $2				}


typeQuant
	:: { Type }
	: typeF 					{ $1					}
	| 'forall' quantVars '.' typeF			{ TForall $2 $4				}

typeF	:: { Type }
	: typeN						{ $1					}
	| typeN ':-' fetters				{ TFetters $3 $1			}

typeN	:: { Type } 
	: typeA						{ $1					}
	| typeA '->' typeN				{ TFun   $1 $3 pure empty		}

	| typeA '-' '(' effect_closure ')' '>' typeN		
	{ let Just k	= takeKindOfType $4
	  in  case k of
	  	KEffect	 -> TFun $1 $7 $4 empty 
		KClosure -> TFun $1 $7 pure $4 }
		
	| typeA '-' '(' effect1 closure1 ')' '>' typeN
	{ TFun   $1 $8 $4 $5 }


typeA	:: { Type }
	: typeZ						{ $1					}
	| qCon typeZ_space				{ TData (vNameT  $1) $2			}	
	| qCon '#' typeZ_space				{ TData (vNameTU $1) $3			}


typeZ	:: { Type }
	: varBase
	{ TVar 	(kindOfVarSpace (Var.nameSpace $1)) 
		(vNameDefaultN NameType $1) }

	| qCon						{ TData (vNameT  $1) []			}
	| qCon '#'					{ TData (vNameTU $1) []			}
	| '()'						{ TData (vNameT $ makeVar "Unit" $1) [] }
	| '(' typeN ')'					{ $2					}
	| '(' 'mutable' typeN ')'			{ TMutable $3				}
	| kindA '_'					{ TWild	$1				}

	| '[' typeN ']'					{ TData primTList [$2]			}
	| '(' typeN ',' typeN_Scp ')'			{ TData (primTTuple (length $4 + 1)) ($2:$4)	}

typeZ_space
	:: { [Type] }
	: typeZ						{ [$1]					}
	| typeZ typeZ_space				{ $1 : $2				}

-----
typeN_Scp
	:: { [Type] }
	: typeN						{ [$1]					}
	| typeN ',' typeN_Scp				{ $1 : $3				}

-----
{-
typeArg_Ss	
	:: { [Type] }
	: typeArg					{ [$1]					}
	| typeArg typeArg_Ss				{ $1 : $2				}

typeArg	
	:: { Type }
	: typeZ						{ $1					}
-}		

-- quantified Vars
quantVars
	:: { [(Var, Kind)] }
	: quantVar					{ [$1]					}
	| quantVar quantVars				{ $1 : $2				}
		
quantVar
	:: { (Var, Kind) }
	:  pVar						{ ( vNameDefaultN NameType $1
							  , kindOfVarSpace (Var.nameSpace $1))		}
	| '(' pVar '::' kind ')'			{ ($2,	$4)					} 

-----------------------
-- Fetter Expressions
-----------------------

fetters	:: { [Fetter] }
	: fetter					{ [$1]					}
	| fetter ',' fetters				{ $1 : $3				}

fetter	:: { Fetter }
	: pVar '=' effect_closure
	{ FLet	(TVar (kindOfVarSpace (Var.nameSpace $1)) $1)
		$3 }

	| qCon trec1_space				{ FConstraint (vNameW $1) $2		}

	
-----------------------
-- Effect Expressions
-----------------------

effect	:: {Effect}
	: pVar						{ TVar KEffect $1			}
	| effectCtor					{ $1					}
	| '!' '{' effect_semi '}'			{ TSum KEffect $3 			}	

effect_semi
	:: { [Effect] }
	: effect					{ [$1]					}
	| effect ';' effect_semi			{ $1 : $3				}

effectCtor
	:: { Effect }					
	: qCon						{ TEffect $1 []				}
	| qCon typeZ_space				{ TEffect $1 $2 			}


-- Effects which can appear on function arrows
effect1	:: { Effect }
	: pVar						{ TVar KEffect (vNameE $1) }
	| '(' effect ')'				{ $2 }


---------------------
-- Closure
---------------------

closure :: { Closure }
	: pVar						{ TVar KClosure (vNameC $1)		}
	| '$' '{' closure_semi '}'			{ TSum KClosure $3			}
	| pVar ':' closureK				{ TFree (vNameV $1) $3			}
	| pVar ':' typeN				{ TFree (vNameV $1) $3 			}
	| pVar '\\' pVar				{ TMask KClosure (TVar KClosure $1) (TVar KClosure $3)	}

closureK 
	:: { Closure }
	: '$' '{' closure_semi '}'			{ TSum  KClosure $3					}
	| pVar '\\' pVar				{ TMask KClosure (TVar KClosure $1) (TTag $3)		}
		
closure_semi
	:: { [Closure] }
	: closure					{ [$1]					}
	| closure ';' closure_semi			{ $1 : $3				}

closure1 :: { Closure }
	: pVar						{ TVar KClosure (vNameC $1) }
	| '(' closure ')'				{ $2 }

-- Either an effect or closure, for function arrow annotations-- 
effect_closure 
	:: { Type }
	: pVar						{ TVar (kindOfVarSpace (Var.nameSpace $1)) $1 }
	| '$' '{' closure_semi '}'			{ TSum KClosure $3			}
	| pVar ':' closureK				{ TFree (vNameV $1) $3			}
	| pVar ':' typeN				{ TFree (vNameV $1) $3 			}
	| pVar '\\' pVar				{ TMask KClosure (TVar KClosure $1) (TTag $3) }
	| effectCtor					{ $1					}
	| '!' '{' effect_semi '}'			{ TSum KEffect $3 			}	


-- A type/region/effect/closure which can be used as a type argument.
trec1 :: { Type }
	: pVar						{ TVar (kindOfVarSpace (Var.nameSpace $1)) $1 	}
	| '$' '{' closure_semi '}'			{ TSum KClosure $3				}
	| '!' '{' effect_semi '}'			{ TSum KEffect $3 				}	
	| '(' trec ')'					{ $2						}
	
trec	:: { Type }
	: trec1						{ $1						}
	| pVar ':' closureK				{ TFree (vNameV $1) $3				}
	| pVar ':' typeN				{ TFree (vNameV $1) $3 				}
	| pVar '\\' pVar				{ TMask KClosure (TVar KClosure $1) (TTag $3) 	}
	| qCon typeZ_space				{ makeTECon (dNameN NameType $1) $2		}	
	| qCon '#' typeZ_space				{ TData (vNameTU $1) $3				}

trec1_space :: { [Type] }
	: trec1						{ [ $1 ] 					}
	| trec1 trec1_space				{ $1 : $2					}

	
---------------------
-- Variables
-- 	q versions are possibly qualified
--	p versions are unqualified (plain)
---------------------

-- A regular variable (not a construct name)
qVar	:: { Var }
	: pVar						{ $1					}
	| MODULENAME '.' pVar				{ $3 { Var.nameModule = makeModule $1 }	}

qVars	:: { [Var] }
qVars	: {- empty -} 					{ []					}
	| qVar qVars					{ $1 : $2				}

qVars_comma
	:: { [Var] }
	: qVar						{ [$1]					}
	| qVar ',' qVars_comma				{ $1 : $3				}

pVar	:: { Var }
	: varBase					{ $1 					}
	| '(' symbol ')'				{ $2					}


pVars_space
	:: { [Var] }
	: pVar						{ [$1]					}
	| pVar pVars_space				{ $1 : $2				}


pVars_space_empty
	:: { [Var] }
	: {- empty -}					{ []					}
	| pVar pVars_space_empty			{ $1 : $2				}
	
pVars_comma
	:: { [Var] }
	: pVar						{ [$1]					}
	| pVar ',' pVars_comma				{ $1 : $3				}

-- A constructor name
qCon	:: { Var }
	: pCon						{ $1					}
	| MODULENAME '.' pCon				{ $3 { Var.nameModule = makeModule $1 }	}

pCon	:: { Var }
	: CON						{ toVar $1 				}

varBase	:: { Var }
	: VAR						{ toVar $1				}
	| 'elaborate'					{ makeVar "elaborate" $1		}
	| 'const'					{ makeVar "const"  $1			}
	| 'mutable'					{ makeVar "mutable" $1			}
	| 'extern'					{ makeVar "extern" $1			}

symbol	:: { Var }
	: SYM						{ toVar $1				}
	| ':'						{ toVar $1				}
	| '*'						{ toVar $1				}
	| '-'						{ toVar $1				}
	| '!'						{ toVar $1				}
	| '@'						{ toVar $1				}
	| '<'						{ toVar $1				}
	| '>'						{ toVar $1				}
	| '/'						{ toVar $1				}
	| '+'						{ toVar $1				}
	| '$'						{ toVar $1				}
	| '%'						{ toVar $1				}


{ -- Start of Happy Haskell code


-- | Callback for happy to use when it's not happy.
happyError ::	[TokenP] -> a
happyError	[]	= dieWithUserError [ErrorParseEnd]
happyError	(x:xs)	= dieWithUserError [ErrorParseBefore x]


-- | Convert a token to a variable
--	We need to undo the lexer tokens here because some of our 
--	"reserved symbols" alias with valid variable names.
--
toVar :: TokenP -> Var
toVar	 tok
 = case token tok of
	K.Var    name	-> Var.loadSpaceQualifier $ makeVar name tok
	K.Con	 name	-> Var.loadSpaceQualifier $ makeVar name tok
	K.Symbol name	-> makeVar name tok
	_ -> case lookup (token tok) toVar_table of
		Just name	-> makeVar name tok
		Nothing		-> panic stage ("toVar: bad token: " ++ show tok)


-- | String representations for these tokens.
toVar_table :: [(Token, String)]
toVar_table = 
	[ (K.Colon,		":")
	, (K.Star,		"*")
	, (K.Dash,		"-")
	, (K.At,		"@")
	, (K.Hash,		"#") 
	, (K.ABra,		"<")
	, (K.AKet,		">") 
	, (K.ForwardSlash,	"/")
	, (K.Plus,		"+")
	, (K.Dot,		".")
	, (K.Dollar,		"$")
	, (K.Tilde,		"~")
	, (K.Percent,		"%") ]

toVarHash space tok
 = let	v	= toVar tok
   in	v	{ Var.name	= (Var.name v ++ "#")
   		, Var.nameSpace	= space }


-- | Make a variable with this name,
--	using the token as the source location for the var.
makeVar :: String -> TokenP -> Var
makeVar    name@(n:_) tok
	= (Var.new name)
	 	{ Var.info	=	
		[ Var.ISourcePos (SourcePos (file tok, line tok, column tok)) ] }

-- | Make either a TData or TEffect constructor, depending on the namespace of the variable
makeTECon :: Var -> [Type] -> Type
makeTECon v ts
 = case Var.nameSpace v of
 	NameType	-> TData v ts
	NameEffect	-> TEffect v ts


-- | Make a module name from this token
makeModule :: TokenP -> Module
makeModule tok
 = case token tok of
 	K.ModuleName names	-> ModuleAbsolute names
	K.Con        name	-> ModuleAbsolute [name]
	_			-> dieWithUserError [ ErrorParse tok "parse error" ]

-- | Force an expresion to be a variable
--	Throw a user error if it's not.
checkVar ::	TokenP -> Exp -> Var
checkVar	tok	  (XVar sp v)	= v
checkVar	tok	  e
 	= dieWithUserError [ErrorParse tok "parse error"]


-- | Make a constant expression from this token
makeConst ::	Bool -> TokenP -> Exp
makeConst	isUnboxed tok
 = let	sp	= SourcePos (file tok, line tok, column tok)
   in   if isUnboxed 
   		then XConst sp $ CConstU $ makeLit tok
		else XConst sp $ CConst  $ makeLit tok
  
  
-- Make a literal from this token.
makeLit :: TokenP -> Literal
makeLit tok 
 = case token tok of
 	K.CInt    i	-> LInt    i
	K.CChar   c	-> LChar   c
	K.CFloat  f	-> LFloat  f
	K.CString s	-> LString s
 

-----
getCIntValue ::	TokenP -> Int
getCIntValue	tok
 = case token tok of
 	K.CInt   i	-> i


-------------------------------------------------------------------
-- Helper functions to use when creating the syntax tree
--

-- | Slurp the source position from this token.
spTP :: TokenP -> SourcePos
spTP    tok
 = SourcePos (file tok, line tok, column tok)


-- | Slurp the source position from this expression.
spX :: Exp -> SourcePos
spX 	= sourcePosX


-- | Slurp the source position from this variable.
spV :: Var -> SourcePos
spV var
 = let	[sp]	= [sp | Var.ISourcePos sp <- Var.info var]
   in	sp

-- | Force the namespace of this variable
--	If it has already been set differently then panic
vNameN :: NameSpace -> Var -> Var
vNameN space v
	| Var.nameSpace v == NameNothing
	= v { Var.nameSpace = space }

	| Var.nameSpace v /= space
	= panic stage
	$ "vNameN: conflicting namespace for variable " % v	% "\n"
	% "   name space was     " % Var.nameSpace v		% "\n"
	% "   tried to set it to " % space			% "\n"
	
	| otherwise
	= v { Var.nameSpace = NameValue }

vNameV		= vNameN NameValue
vNameT		= vNameN NameType
vNameR		= vNameN NameRegion
vNameE		= vNameN NameEffect
vNameC		= vNameN NameClosure
vNameW		= vNameN NameClass
vNameF		= vNameN NameField

vNameTU v	= v 
		{ Var.name 	= (Var.name v ++ "#")
   		, Var.nameSpace = NameType }

-- | If the namespace of this var is NameNothing, set it to this one
dNameN :: NameSpace -> Var -> Var
dNameN space v
 	| Var.nameSpace v == NameNothing
	= v { Var.nameSpace = space }
	
	| otherwise
	= v


-- | If the var has no namespace set, then give it this one.
vNameDefaultN	:: NameSpace -> Var -> Var
vNameDefaultN space var
 = case Var.nameSpace var of
 	NameNothing	-> var { Var.nameSpace = space }
	_		-> var



-- | Decide on the kind of a type var from it's namespace
kindOfVarSpace :: NameSpace -> Kind
kindOfVarSpace space
 = case space of
 	NameNothing	-> KData
	NameRegion	-> KRegion
	NameEffect	-> KEffect
	NameClosure	-> KClosure
	
} -- end of Happy Haskell code



