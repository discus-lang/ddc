-----
-- Source.Parser
--
-- Summary:
--	Do the minimal amount of work in the parser. 
--	Try not to do any desugaring here.
--	Source parse tree should preserve structure of program as entered by user.
--	This way we'll get better error messages during type checking.
--
-- Errors:
--	deathParseEnd
--	deathParseBefore
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
import Type.Util		(pure, empty)

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
	':*'		{ TokenP { token = K.HasTypeQuant  } }
	
	':::'		{ TokenP { token = K.HasTypeExact } }
	'::*'		{ TokenP { token = K.HasTypeExactQuant } }
	
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
	
	'\\'		{ TokenP { token = K.BSlash	} }
	'`'		{ TokenP { token = K.BTick	} }
	'='		{ TokenP { token = K.Equals	} }
	','		{ TokenP { token = K.Comma	} }	
	':'		{ TokenP { token = K.Colon	} }
	';'		{ TokenP { token = K.SColon	} }
	'|'		{ TokenP { token = K.Bar	} }
	'.'		{ TokenP { token = K.Dot	} }

	'#'		{ TokenP { token = K.Hash	} }
	'*'		{ TokenP { token = K.Star	} }
	'-'		{ TokenP { token = K.Dash	} }
	'+'		{ TokenP { token = K.Plus	} }
	'%'		{ TokenP { token = K.Percent	} }
	'@'		{ TokenP { token = K.At		} }
	'!'		{ TokenP { token = K.Bang	} }	
	'/'		{ TokenP { token = K.FSlash	} }
	'$'		{ TokenP { token = K.Dollar	} }
	'_'		{ TokenP { token = K.Underscore	} }
	'^'		{ TokenP { token = K.Hat	} }
	'..'		{ TokenP { token = K.DotDot	} }
	'<-'		{ TokenP { token = K.LeftArrow	} }
	'<@-'		{ TokenP { token = K.LeftArrowLazy } }
		
	CON		{ TokenP { token = K.Tycon  _	} }
	VAR		{ TokenP { token = K.Var    _	} }
	SYM		{ TokenP { token = K.Symbol _	} }
	
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
	| 'import' '{' module_CsZ '}' 			{ [PImportModule   $3]				}

	| 'foreign' foreign				{ [PForeign $2]					}


	| infixMode INT symbolList ';'			{ [PInfix    $1 (getCIntValue $2) $3]		}
	| dataDef ';'					{ [$1]						}

	| 'effect' '!' CON '::' kind ';'		{ [PEffect  (toVarE $3) $5]			}
	| 'region' '%' sVar ';'				{ [PRegion  $3]					}

	-- class defs
	| 'class' CON '::' kind ';' 
	{ [PClass   (toVarC $2) $4] }

	| 'class' CON VAR classCstrs 'where' '{' typeSig_Ss '}' 	
	{ [PClassDict (toVarC $2) [(toVarT $3)] $4 $7 ] }

	| 'instance' CON typeZ_Ssp instCstrs 'where' '{' binds '}'
	{ [PClassInst (toVarC $2) $3 $4 $7 ] }

	-- projection defs
	| 'project' typeA 'where' '{' stmtSigBinds '}'
	{ [PProjDict $2 $5]		}

	-- type sigs
	| stmtTypeSig					
	{ [PType (t3_1 $1) (t3_2 $1) (t3_3 $1)]		}
		
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
	: 'extern' mString sVar '::' type ';'		 { OExtern $2 $3 $5 Nothing	}
	| 'extern' mString sVar '::' type ':$' typeN ';' { OExtern $2 $3 $5 (Just $7) 	}


mString		
	:: { Maybe String }
	: {- empty -}				{ Nothing					}
	| STRING				{ Just ((\(K.CString s) -> s) (token $1))	}

-----
-- Modules
--
module_CsZ	
	:: { [Module] }
	: {- empty -}				{ [] 					}
	| module				{ [$1] 					}
	| module ';' module_CsZ			{ $1 : $3 				}	

module		
	:: { Module }
	: module_VsD				{ ModuleAbsolute $1			}
--	| '.' module_VsD			{ ModuleRelative $2			}

module_VsD	
	:: { [String] }
	: CON					{ [Var.name $ toVar $1]			}
	| CON '.' module_VsD			{ (Var.name $ toVar $1) : $3		}

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
	: sVar '::' type ':$' typeN ';'		{ PImportExtern $1 $3 (Just $5)		}

externSigs
	:: { [Top] }
	: externSig				{ [$1]					}
	| externSig externSigs			{ $1 : $2				}

typeSig
	:: { ([Var], Type) }
	: var_Csp '::' type ';'			{ ($1, $3)				}
--	| '(' symbol ')' '::' type ';'		{ ([vNameV $2], $5)			}
			
typeSig_Ss
	:: { [([Var], Type)] }
	: typeSig				{ [$1]					}
	| typeSig typeSig_Ss			{ $1 : $2				}
			

-----
-- ClassCstrs
--	Type class constraints
--
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
	:  CON vars				{ (toVarC $1, $2)			}
		
-----
-- InstCstrs
--	Instance constraints
--
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
	: CON typeZ_Ssp				{ (toVarC $1, $2)			}


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
	| '\\' '.' var expAppZs			{ XLambdaProj   (spTP $1) (JField  $3) $4	}

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

	| '`' cVar '`'				{ XOp  		(spTP $1) (vNameV $2)	}
--	| '(' symbol ')'			{ XVar (vNameV $2)			}
		
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

	| CON					{ XVar		(spTP $1) (toVarV $1)	}


expA	
	:: { Exp }				

	: '(' exp ')'				{ $2					}

	-- projections
	| expA '.' var				{ XProj (spTP $2) $1 (JField  $3)	}
	| expA '#' var				{ XProj (spTP $2) $1 (JFieldR $3)	}

	| expA '.' '{' var     '}'		{ XProj (spTP $2) $1 (JAttr   $4)	}
	| expA '.' '{' VAR '#' '}'		{ XProj (spTP $2) $1 (JAttr   (toVarHash NameAttr $4)) }

	| expA '.' '(' exp ')'			{ XProj (spTP $2) $1 (JIndex  $4) 	}
	| expA '#' '(' exp ')'			{ XProj (spTP $2) $1 (JIndexR $4)	}

	-- object syntax
	| '^' var				{ XObjVar   (spTP $1) (vNameV $2)	}
	| '_' var				{ XObjField (spTP $1) (vNameV $2)	}

	| 'break'				{ XBreak  (spTP $1)			}
	| '()'					{ XVar	  (spTP $1) (makeVar "Unit" $1)	}

	| mSVar					{ XVar 	  (spV $1) (vNameV $1)		}

		


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
	|  stmtTypeSig				{ SSig (t3_1 $1) (t3_2 $1) (t3_3 $1)	}

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
	|  stmtTypeSig				{ SSig (t3_1 $1) (t3_2 $1) (t3_3 $1)	}

stmtTypeSig
	:: { (SourcePos, Var, Type) }
	: sVar '::' type ';'			{ (spTP $2, vNameV $1, TSig (TQuant $3)) 	}
	| sVar ':*' type ';'			{ (spTP $2, vNameV $1, TSig $3) 		}

	| sVar ':::' type ';'			{ (spTP $2, vNameV $1, TSigExact (TQuant $3)) 	}
	| sVar '::*' type ';'			{ (spTP $2, vNameV $1, TSigExact $3) 		}
		


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

	| con '{' '}'				{ WConLabel $1 []			}
	| con '{' labelPat_Cs '}'		{ WConLabel $1 $3			}


labelPat_Cs
	:: { [(Label, Pat)] }
	: labelPat				{ [$1]					}
	| labelPat ',' labelPat_Cs		{ $1 : $3				}
		
labelPat
	:: { (Label, Pat) }
	: '.' var '=' var			{ (LVar $2, WVar $4)				}
	| '.' INT '=' var			{ (LIndex (getCIntValue $2), WVar $4)	}
		

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
	: 'data' CON varTREC_SsE			{ PData (toVarT $2) $3 []		}
	| 'data' CON varTREC_SsE '=' dataConss		{ PData (toVarT $2) $3 $5 		}

	| 'data' CON '#' varTREC_SsE			{ PData (toVarHash NameType $2) $4 []	}
		
	| 'data' CON '#' varTREC_SsE 'foreign' STRING 	
	{ 
		let	
			K.CString name	= token $6
			var		= (toVarHash NameType $2) { Var.info = [Var.ISeaName name] }
	  	in	PData var $4 [] 
	}
		
dataConss
	:: { [(Var, [DataField Exp Type])] }
	: dataCons					{ [$1]					}
	| dataCons '|' dataConss			{ $1 : $3				}


dataCons
	:: { (Var, [DataField Exp Type]) }
	: con						{ ($1, [])				}
	| con dataType_Ss				{ ($1, $2)				}
	| con '{' dataField_Ss '}'			{ ($1, $3)				}
		
dataField
	:: { DataField Exp Type }
	: type ';'					{ DataField 
								{ dPrimary	= True
								, dLabel	= Nothing
								, dType		= $1
								, dInit		= Nothing } }
								

	| var '::' type ';'				{ DataField 
								{ dPrimary	= True	
								, dLabel 	= Just $1
								, dType		= $3
								, dInit		= Nothing } }
								
	| '.' var '::' type dataInit ';'		{ DataField 
								{ dPrimary	= False
								, dLabel	= Just $2
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
								

varTREC
	:: { Var }
	: VAR						{ toVarT $1				}
	| '%' VAR					{ toVarR $2				}
	| '!' VAR					{ toVarE $2				}
	| '$' VAR					{ toVarO $2				}

varTREC_SsE	
	:: { [Var] }
	: {- empty -}					{ []					}
	| varTREC varTREC_SsE				{ $1 : $2				}


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

	| typeA '-' '(' effect ')' '>' typeN		
	{ TFun   $1 $7 $4 empty 	}

	| typeA '-' '(' closure ')' '>' typeN		
	{ TFun   $1 $7 pure $4 	}

	| typeA '-' '(' effect1 closure ')' '>' typeN
	{ TFun   $1 $8 $4 $5 }


typeA	:: { Type }
	: typeZ						{ $1					}
	| mCon typeArg_Ss				{ TData (vNameT  $1) $2			}	
	| mCon '#' typeArg_Ss				{ TData (vNameTU $1) $3		}


typeZ	:: { Type }
	: VAR 						{ TVar KData (toVarT  $1)			}
	| mCon						{ TData (vNameT  $1) []			}
	| mCon '#'					{ TData (vNameTU $1) []		}
	| '()'						{ TData (makeTVar "Unit" $1)  []		}
	| '(' typeN ')'					{ $2					}
	| '(' 'mutable' typeN ')'			{ TMutable $3				}
	| '(' kind ')'					{ TWild	$2				}

	| '[' typeN ']'					{ TData primTList [$2]			}
	| '(' typeN ',' typeN_Scp ')'			{ TData (primTTuple (length $4 + 1)) ($2:$4)	}

typeZ_Ssp
	:: { [Type] }
	: typeZ						{ [$1]					}
	| typeZ typeZ_Ssp				{ $1 : $2				}

-----
typeN_Scp
	:: { [Type] }
	: typeN						{ [$1]					}
	| typeN ',' typeN_Scp				{ $1 : $3				}

-----
typeArg_Ss	
	:: { [Type] }
	: typeArg					{ [$1]					}
	| typeArg typeArg_Ss				{ $1 : $2				}

typeArg	
	:: { Type }
	: typeZ						{ $1					}
	| '%' sVar					{ TVar KRegion $2 { Var.nameSpace = NameRegion }	}
	| '!' sVar					{ TVar KEffect $2 { Var.nameSpace = NameEffect }	}
	| '$' sVar					{ TVar KClosure $2 { Var.nameSpace = NameClosure }	}
		

-----
quantVars
	:: { [(Var, Kind)] }
	: quantVar					{ [$1]					}
	| quantVar quantVars				{ $1 : $2				}
		
quantVar
	:: { (Var, Kind) }
	:  sVar						{ ($1 { Var.nameSpace = NameType },	KData)		}
	| '(' sVar '::' kind ')'			{ ($2 { Var.nameSpace = NameType },	$4)		} 
	| '%' sVar					{ ($2 { Var.nameSpace = NameRegion },	KRegion)	}
	| '!' sVar					{ ($2 { Var.nameSpace = NameEffect },	KEffect)	}
	| '$' sVar					{ ($2 { Var.nameSpace = NameClosure },	KClosure)	} 

kind	:: { Kind }
	: kindA						{ $1					}
	| kindA '->' kind				{ KFun $1 $3				}

kindA	:: { Kind }
	: '*'						{ KData					}
	| '%'						{ KRegion				}
	| '!'						{ KEffect				}
	| '+'						{ KFetter				}

-----
fetters	:: { [Fetter] }
	: fetter					{ [$1]					}
	| fetter ',' fetters				{ $1 : $3				}

fetter	:: { Fetter }
	: '!' VAR '=' effectSum				{ FLet (TVar KEffect  $ toVarE $2) $4	}
	| '$' VAR '=' closureSum			{ FLet (TVar KClosure $ toVarO $2) $4	}

	| CON '=' effectSum '|' closureSum		{ FFunInfo (toVar $1) $3   $5 		}
	| CON '=' effectSum				{ FFunInfo (toVar $1) $3   empty	}

	| CON '=' closureSum				{ FFunInfo (toVar $1) pure $3		}

	| mCon typeArg_Ss				{ FConstraint (vNameC $1) $2		}


-----

	
-----------------------
-- Effect Expressions
-----------------------

effect	:: {Effect}
	: effectVar					{ $1 }
	| effectCtor					{ $1 }
	| effectSum					{ $1 }

effectVar 
	:: { Effect }
	: '!' VAR					{ TVar KEffect (toVarE $2)		}

effectCtor
	:: { Effect }					
	: effectCon					{ TEffect $1 []				}
	| effectCon typeArg_Ss				{ TEffect $1 $2 			}

effectCon
	:: { Var }
	: effectConPs					{ makeModuleVar $1			}

effectConPs
	:: { [Var] }
	: CON '.' effectConPs				{ toVarE $1 : $3			}
	| '!' CON					{ [toVarE $2] 				}

effectSum
	:: { Effect }
	: '!' '{' effect_cs '}'				{ TSum KEffect $3			}

effect_cs
	:: { [Effect] }
	: effect					{ [$1]					}
	| effect ';' effect_cs				{ $1 : $3				}

effect1	:: { Effect }
	: effectVar					{ $1 }
	| '(' effect ')'				{ $2 }

effectCtor1
	:: { Effect }
	: effectCon					{ TEffect $1 []				}


---------------------
-- Closure
---------------------

closure :: { Closure }
	: closureSum					{ $1 }
	| closurePart					{ $1 }

closureSum
	:: { Closure }
	: '$' '{' closurePart_Cs '}'			{ TSum KEffect $3			}
		
closurePart
	:: { Closure }
	: '$' cVar					{ TVar KEffect   ($2 { Var.nameSpace = NameValue})  }	
	| cVar '::' type				{ TFree ($1 { Var.nameSpace = NameValue}) $3 }
		
closurePart_Cs
	:: { [Closure] }
	: closurePart					{ [$1]					}
	| closurePart ';' closurePart_Cs		{ $1 : $3				}


---------------------
-- Utilities
---------------------

-----
mSVar	:: { Var }
	: mSVar2					{ $1					}
	| mCon2 '.' mSVar2				{ makeModuleVar ($1 ++ [$3])		}

mSVar2	:: { Var }
	: var						{ $1					}
	| '(' symbol ')'				{ $2					}


mVar	:: { Var }
	: sVar						{ $1					}
	| mCon2 '.' sVar				{ makeModuleVar ($1 ++ [$3])		}

mCon	:: { Var }
	: mCon2						{ makeModuleVar $1			}

mCon2	:: { [Var] }
	: CON						{ [toVar $1]				}
	| mCon2 '.' CON					{ $1 ++ [toVar $3]			}

-----
var	:: { Var }
	: VAR						{ toVar $1				}
--	| 'module'					{ makeVar "module" $1			}
	| 'elaborate'					{ makeVar "elaborate" $1		}
	| 'const'					{ makeVar "const"  $1			}
	| 'mutable'					{ makeVar "mutable" $1			}
	| 'extern'					{ makeVar "extern" $1			}
--	| 'ccall'					{ makeVar "ccall"  $1			}		

vars	:: { [Var] }
vars	: {- empty -} 					{ []					}
	| var vars					{ $1 : $2				}

var_Csp	:: { [Var] }
	: mVar						{ [$1]					}
	| mVar ',' var_Csp				{ $1 : $3				}

con	:: { Var }
	: CON						{ toVar $1				}

cVar	:: { Var }
	: var						{ $1 					}
	| con						{ $1					}

sVar	:: { Var }
	: var						{ $1					}
	| con						{ $1					}
	| '(' symbol ')'				{ $2					}

-----
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


{
-----
happyError ::	[TokenP] -> a

happyError	[]	= death [ErrorParseEnd]
happyError	(x:xs)	= death [ErrorParseBefore x]


-----
toVar :: TokenP -> Var
toVar	 tok
 = case token tok of
	K.Var    name	-> makeVar name tok
	K.Tycon  name	-> (makeVar name tok)
	K.Symbol name	-> (makeVar name tok)
	_		-> 
		(makeVar name tok) 
		 where	name =	fromMaybe (panic stage ("toVar: bad token: " ++ show tok))
			  		  (lookup (token tok) toVar_table)

toVar_table :: [(Token, String)]
toVar_table = 
	[ (K.Colon,  ":")
	, (K.Star,   "*")
	, (K.Dash,   "-")
	, (K.At,     "@")
	, (K.Hash,   "#") 
	, (K.ABra,   "<")
	, (K.AKet,   ">") 
	, (K.FSlash, "/")
	, (K.Plus,   "+")
	, (K.Dot,    ".")
	, (K.Dollar, "$")
	, (K.Tilde,  "~")
	, (K.Percent, "%") ]


-----
-- TODO: change these to have the same form.

toVarT	tok	= (toVar tok) { Var.nameSpace = NameType }
toVarE	tok	= (toVar tok) { Var.nameSpace = NameEffect }
toVarR	tok	= (toVar tok) { Var.nameSpace = NameRegion }
toVarV	tok 	= (toVar tok) { Var.nameSpace = NameValue }
toVarM	tok	= (toVar tok) { Var.nameSpace = NameModule }
toVarC	tok	= (toVar tok) { Var.nameSpace = NameClass }
toVarO	tok	= (toVar tok) { Var.nameSpace = NameClosure }

toVarHash space tok
 = let	v	= toVar tok
   in	v	{ Var.name	= (Var.name v ++ "#")
   		, Var.nameSpace	= space }

vNameV v	= v { Var.nameSpace = NameValue }
vNameT v	= v { Var.nameSpace = NameType }
vNameE v	= v { Var.nameSpace = NameEffect }
vNameC v	= v { Var.nameSpace = NameClosure }

vNameTU v	= v 
		{ Var.name 	= (Var.name v ++ "#")
   		, Var.nameSpace = NameType }


-----
makeVar :: String -> TokenP -> Var
makeVar    name@(n:_)	     tok
 = 	(Var.new name)
 		{ Var.info	=
			[  Var.ISourcePos (SourcePos (file tok, line tok, column tok)) ] }

makeTVar :: String -> TokenP -> Var
makeTVar    name      tok
 = 	(makeVar name tok) { Var.nameSpace = Var.NameType }


makeModuleVar :: [Var] -> Var
makeModuleVar vs
 = let	Just ms	= takeInit vs
 	Just v	= takeLast vs
	
   in	case ms of
   		[]	-> v { Var.nameModule = ModuleNil }
		_	-> v { Var.nameModule = ModuleAbsolute (map Var.name ms) }
	
-----
checkVar ::	TokenP -> Exp -> Var
checkVar	tok	  (XVar sp v)	= v
checkVar	tok	  e
 	= death [ErrorParse tok "parse error"]

-----
makeConst ::	Bool -> TokenP -> Exp
makeConst	isUnboxed tok
 = let	sp	= SourcePos (file tok, line tok, column tok)
   in   if isUnboxed 
   		then XConst sp $ CConstU $ makeLit tok
		else XConst sp $ CConst  $ makeLit tok
   
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


spTP :: TokenP -> SourcePos
spTP    tok
 = SourcePos (file tok, line tok, column tok)


spX :: Exp -> SourcePos
spX 	= sourcePosX

spV :: Var -> SourcePos
spV var
 = let	[sp]	= [sp | Var.ISourcePos sp <- Var.info var]
   in	sp

-----
gatherEither :: [Either a b] -> ([a], [b])
gatherEither	xx		= (reverse aa, reverse bb)
 where
 	(aa, bb)	= gatherEither' ([], []) xx


gatherEither' :: ([a], [b]) ->	[Either a b]	-> ([a], [b])
gatherEither'	(aa, bb)	[]		= (aa, bb)
gatherEither'	(aa, bb)	(e:es)
 = case e of
 	Left  a	-> gatherEither' (a : aa, bb) es
	Right b	-> gatherEither' (aa, b : bb) es
	



}



