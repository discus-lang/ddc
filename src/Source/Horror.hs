
module Source.Horror
where

import Source.Exp

-- Top
isPData xx	= case xx of { PData{}	-> True;	_ -> False; }
isPStmt	xx	= case xx of { PStmt{}	-> True;	_ -> False; }
isPClassDict xx	= case xx of { PClassDict{} -> True;	_ -> False; }
isPClassInst xx	= case xx of { PClassInst{} -> True;	_ -> False; }
isPImportExtern xx = case xx of { PImportExtern{} -> True;	_ -> False; }
isPInfix xx	= case xx of { PInfix{}	-> True;	_ -> False; }

-- Exp
isXVar xx	= case xx of { XVar{}	-> True; 	_ -> False; }
isXLambda xx	= case xx of { XLambda{} -> True;	_ -> False; }

isXApp xx	= case xx of { XApp{} 	-> True; 	_ -> False; }
isXAnnot xx	= case xx of { XAnnot{}	-> True;	_ -> False; }
isXUnit xx	= case xx of { XUnit{}	-> True;	_ -> False; }
isXLet xx	= case xx of { XLet{}	-> True;	_ -> False; }
isXCase xx	= case xx of { XCase{}	-> True;	_ -> False; }
isXDo xx	= case xx of { XDo{}	-> True;	_ -> False; }

isXCaseE xx	= case xx of { XCaseE{}	-> True; 	_ -> False; }

-- Stmt
-- isSBind xx	= case xx of { SBind{}	-> True;	_ -> False; }
