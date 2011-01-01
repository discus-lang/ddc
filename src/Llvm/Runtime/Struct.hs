{-# OPTIONS -fwarn-unused-imports -fno-warn-type-defaults #-}

-- | Functions for defining DDC structs so that they can interact with the C
-- runtime. Struct definitions in the file need to be kept in sync with the
-- ones in runtime/Object.h.

module Llvm.Runtime.Struct
	( sizeOfLlvmType
	, offsetOfIndex

	, LlvmStructField (..)
	, LlvmStructDesc
	, mkLlvmStructDesc
	, llvmTypeOfStruct
	, structFieldLookup )
where

import DDC.Main.Error

import Llvm

import qualified Config.Config		as Config

import qualified Data.Map		as Map


stage = "Llvm.Runtime.Struct"


-- | A structure can be defined as a list of LlvmStructField fields with optional
-- padding to force the correct alignment.
--
-- The APadToX values only add padding if, due to previous fields in the struct,
-- The next field would not start at the required byte offset.


data LlvmStructField
	= AField String LlvmType	-- Field name and type.
	| APadTo2			-- Pad next field to a 2 byte offset.
	| APadTo4			-- Pad next field to a 4 byte offset.
	| APadTo8			-- Pad next field to a 8 byte offset.

	| APadTo8If64			-- Pad next field to a 8 byte offset only
					-- for 64 bit.

-- | A structure defined by a list if LlvmStructField fields can be turned into
-- an LlvmStructDesc which gives the struct a name, an LlvmType derived from
-- the LlvmStructField list description (adding padding fields where needed)
-- and a map from the field names to the (index, LlvmType) pair.

data LlvmStructDesc
 = LlvmStructDesc
	String					-- Struct name
	LlvmType				-- LlvmType of this struct
	(Map.Map String (Int, LlvmType))	-- name -> field index map



-- Temporary data type used for the construction of LlvmStructDesc
data PaddedStruct
	= Field String (Int, LlvmType)	-- Field name and (index, type).
	| Pad LlvmType			-- Pad with the specified type.


llvmTypeOfPadded :: PaddedStruct -> LlvmType
llvmTypeOfPadded field
 = case field of
	Field _ (_, t)	-> t
	Pad t		-> t


mkConcrete :: (Int, [PaddedStruct]) -> LlvmStructField -> (Int, [PaddedStruct])
mkConcrete (n, accum) f@(AField name lt)
 =	(n + sizeOfLlvmType lt, Field name (length accum, lt) : accum)

mkConcrete (n, accum) APadTo2
 | mod n 2 == 0
 =	(n, accum)

 | otherwise
 =	(n + 1, Pad i8 : accum)

mkConcrete (n, accum) APadTo4
 | mod n 4 == 0
 =	(n, accum)

 | otherwise
 = 	let padding = 4 - mod n 4
	in (n + padding, Pad (LMArray padding i8) : accum)

mkConcrete (n, accum) APadTo8
 | mod n 8 == 0
 =	(n, accum)

 | otherwise
 = 	let padding = 8 - mod n 8
	in (n + padding, Pad (LMArray padding i8) : accum)


mkConcrete (n, accum) APadTo8If64
 | Config.pointerBytes == 4
 =	(n, accum)


mkConcrete (n, accum) APadTo8If64
 | Config.pointerBytes == 8 && mod n 8 == 0
 =	(n, accum)

 | Config.pointerBytes == 8
 = 	let padding = 8 - mod n 8
	in (n + padding, Pad (LMArray padding i8) : accum)



-- | Turn an struct specified as an LlvmStructField list into an LlvmStructDesc.
mkLlvmStructDesc :: String -> [LlvmStructField] -> LlvmStructDesc
mkLlvmStructDesc name fields
 = do	let concrete = reverse $ snd $ foldl mkConcrete (0, []) fields
	LlvmStructDesc name
		(LMStruct (map llvmTypeOfPadded concrete))
		(Map.fromList (concatMap nameIndexType concrete))

nameIndexType :: PaddedStruct -> [ (String, (Int, LlvmType)) ]
nameIndexType ps
 = case ps of
	Field n (i, t)	-> [ (n, (i, t)) ]
	Pad _		-> []


-- | Retrieve the struct's LlvmType from the LlvmStructDesc.
llvmTypeOfStruct :: LlvmStructDesc -> LlvmType
llvmTypeOfStruct (LlvmStructDesc _ t _) = t


-- | Given and LlvmStructDesc and the name of a field within the LlvmStructDesc,
-- retrieve a field's index with the struct and its LlvmType.
structFieldLookup :: LlvmStructDesc -> String -> (Int, LlvmType)
structFieldLookup (LlvmStructDesc name _ map) field
 = case Map.lookup field map of
	Just x -> x
	Nothing -> panic stage $ "findIndex '" ++ field ++ "' in struct '" ++ name ++ "' failed."


--------------------------------------------------------------------------------

-- | Calculate the size in bytes of an LlvmType.
sizeOfLlvmType :: LlvmType -> Int
sizeOfLlvmType t
 = case t of
	LMInt bits	-> div bits 8
	LMFloat		-> 4
	LMDouble	-> 8
	LMFloat80	-> 10
	LMFloat128	-> 16
	LMPointer _	-> Config.pointerBytes
	LMArray n t	-> n * sizeOfLlvmType t
	LMLabel		-> panic stage "sizeOfLlvmType LMLabel"
	LMVoid		-> panic stage "sizeOfLlvmType LMVoid"
	LMStruct t	-> sum $ map sizeOfLlvmType t
	LMAlias (_, t)	-> sizeOfLlvmType t


-- | Given an LlvmType containing a struct and an index, calculate the offset in
-- bytes of the field with the specifide index within the struct.
offsetOfIndex :: LlvmType -> Int -> Int
offsetOfIndex _ 0 = 0
offsetOfIndex typ i
 | i > 0
 = case typ of
	LMLabel		-> panic stage "offsetOfIndex LMLabel"
	LMVoid		-> panic stage "offsetOfIndex LMVoid"
	LMStruct t	-> sum $ take i $ map sizeOfLlvmType t
	LMAlias (_, t)	-> offsetOfIndex t i
	_		-> 0
