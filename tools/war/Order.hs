
-- | Order to compile lib modules in
--	TODO: should replace this by a static file

module Order 
	(libraryOrder)
	
where

-----
libraryOrder
	= map (\s -> "library/" ++ s ++ ".ds")
	$ libraryModules
	
libraryModules = 
	[ "Base"
	, "Data/Ref"
	, "Base/Thunk"
	, "Data/Bool"
	, "Class/Copy"
	, "Class/Eq"
	, "Class/Ord"
	, "Class/Num"
	, "Class/Update"
	, "Data/Word32U"
	, "Data/Int32U"
	, "Data/Int"
	, "Data/String"
	, "Data/Float32U"
	, "Data/Float"
	, "Data/Tuple"
	, "Data/Char"
	, "Data/Maybe"
	, "System/Error"
	, "System/Console"
	, "Data/List"
	, "Data/Either"
	, "Data/Set"
	, "Class/Out"
	, "Class/Show"
	, "Control/Imperative"
	, "Control/Exception"
	, "Data/Array"
	, "Data/ArrayU"
	, "DDC/Runtime"
	, "System/File"
	, "Prelude" 
	, "Math/Constants"
	, "Math/Vec2"
	, "Math/Matrix/Matrix33" 
	, "Graphics/Primitive"
	, "Graphics/Shape"
	, "Graphics/TinyPTC" 
	, "Graphics/Raster/Bresenham"
	, "Graphics/Frame"
	, "Graphics/Render" ]

{-	, "DDC/Source/Token"
	, "DDC/Source/Lexer" ]
-}	
