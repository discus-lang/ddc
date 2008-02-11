
module Source.Separate
(
	separate,
	TopS(..)
)

where

import Source.Exp

-----
-- TopS / separate
--	Helps break up the top level defs into their different kinds.
--
data TopS a
	= TopS
	{ tForeign	:: [Top a]
	, tImportExtern	:: [Top a]
	, tTypeDecl	:: [Top a]
	, tData		:: [Top a]
	, tEffect	:: [Top a]
	, tRegion	:: [Top a]
	, tClass	:: [Top a]
	, tClassDict	:: [Top a]
	, tStmt		:: [Top a]
	, tOther	:: [Top a] }

     
initTopS	:: TopS a
initTopS 
	= TopS 
	{ tForeign	= []
	, tImportExtern	= []
	, tTypeDecl	= []
	, tData		= []
	, tEffect	= []
	, tRegion	= []
	, tClass	= []
	, tClassDict	= []
	, tStmt		= []
	, tOther	= [] }


-----
separate ::	[Top a] -> TopS a
separate	ts	= tops'
 where
	tops	= foldl separate' initTopS ts
	
	-- reverse the defs so they come out in an order similar to the way they were defined.
	tops'	= TopS
		{ tForeign	= reverse $ tForeign	  tops
		, tImportExtern	= reverse $ tImportExtern tops
		, tTypeDecl	= reverse $ tTypeDecl	  tops
		, tData		= reverse $ tData	  tops
		, tEffect	= reverse $ tEffect	  tops
		, tRegion	= reverse $ tRegion	  tops
		, tClass	= reverse $ tClass	  tops
		, tClassDict	= reverse $ tClassDict	  tops
		, tStmt		= reverse $ tStmt	  tops
		, tOther	= reverse $ tOther	  tops }


separate' :: 	TopS a -> Top a	-> TopS a
separate'	tops	t
 = case t of
	PForeign{}		-> tops { tForeign	= t : tForeign tops }
 	PImportExtern{}		-> tops { tImportExtern = t : tImportExtern tops }
	PType{} 		-> tops { tTypeDecl	= t : tTypeDecl	tops }
	PData{}			-> tops { tData		= t : tData	tops }
	PEffect{}		-> tops { tEffect	= t : tEffect   tops }
	PRegion{}		-> tops { tRegion	= t : tRegion   tops }
	PClass{}		-> tops { tClass	= t : tClass	tops }
	PClassDict{}		-> tops { tClassDict	= t : tClassDict  tops }
	PStmt{}			-> tops { tStmt		= t : tStmt	tops }
	_			-> tops { tOther	= t : tOther    tops }










