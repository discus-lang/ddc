
module Source.Horror
where

import Source.Exp


-- Exp
isXVar xx	= case xx of { XVar{}	-> True; 	_ -> False; }
isXApp xx	= case xx of { XApp{} 	-> True; 	_ -> False; }
isXLet xx	= case xx of { XLet{}	-> True;	_ -> False; }
isXCase xx	= case xx of { XCase{}	-> True;	_ -> False; }
isXDo xx	= case xx of { XDo{}	-> True;	_ -> False; }

