
-- | Order to compile lib modules in
--	TODO: should replace this by a static file

module Order 
	(libraryOrder)
	
where

import Bits

-----
libraryOrder args
	= map (\s -> "library/" ++ s ++ ".ds")
	$ (libraryModules args)
	
libraryModules args = 
	[ "Base"
	, "Data/Ref"
	, "Base/Thunk"
	, "Data/Function"
	, "Data/Bool"
	, "Class/Copy"
	, "Class/Eq"
	, "Class/Ord"
	, "Class/Num"
	, "Class/Update"
	, "Class/Foldable"
	, "Class/Functor"
	, "Class/Monad"
	, "Data/Ptr"
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
	, "Math/Vec3"
	, "Math/Matrix33" 
	, "Graphics/Primitive"
	, "Graphics/Shape" 
	, "Graphics/Raster/Bresenham" ]

	++ if elem ArgOmitX11 args 
		then []
		else libraryX11

libraryX11 
 = 	[ "Graphics/TinyPTC" 
	, "Graphics/Frame"
	, "Graphics/Render" ]

