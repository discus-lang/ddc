{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Abstract block structured documents.
--	Whereas a `Str` is just a flat slab of text, a `Doc` contains some
--	internal structure that we can write to file then load back in.
--
module DDC.Util.Doc
	( Doc(..)
	, Docable(..) 
	, dNodeIfElems
	, pprDocIndentedWithNewLines)
where
import DDC.Util.Pretty
import DDC.Util.Container
import Data.Foldable
import Data.Set			(Set)
import Data.Sequence		(Seq)
import Prelude			hiding (foldr)
import qualified Data.Set	as Set


-- | An abstract block structured document.
data Doc str
	= -- | An empty document, with nothing in it.
	  DBlank
	
	-- | A document node \/ section.
	| DNode
		{ docNodeName		:: String
		, docNodeContents	:: Doc str }

	-- | A compund document.
	| DList [Doc str]

	-- | String data.
	| DLeaf str
	deriving Show


-- | Something we can produce a document of.
class Docable a str | a -> str where
	doc 	:: a -> Doc str

instance Docable a str => Docable [a] str
 where	doc xx	= DList $ map doc xx

instance Docable a str => Docable (Seq a) str
 where	doc xx	= DList $ map doc $ foldr (:) [] xx

instance Docable a str => Docable (Set a) str 
 where	doc x	= DList $ map doc $ Set.toList x



-- | If this `Container` has elements, 
--	then create a new `DNode` with the given tag, 
--	otherwise `DBlank`.
dNodeIfElems
	:: (Container c, Docable a str)
	=> String
	-> c a 
	-> Doc str

dNodeIfElems tag cont
 = if isEmpty cont
	then DBlank
	else (DNode tag $ doc $ elemsList cont)


-- | Pretty print this `Doc`, 
--	adding blank lines after tags in the given set (for niceness).
pprDocIndentedWithNewLines
	:: forall mode
	.  Int				-- ^ When >0, put newlines after nodes.
	-> Doc (PrettyM mode) 		-- ^ `Doc` to pretty print.
	-> PrettyM mode

pprDocIndentedWithNewLines depth dd
 = case dd of
	DBlank
	 -> blank

	DNode tag dd'
	 | docIsEmpty dd'	
	 -> tag % "."

	DNode tag d@(DLeaf{})
	 | length tag <= 6
	 -> padL 7 (tag ++ ":") %> (" " % pprDocIndentedWithNewLines (depth - 1) d)
	  % (if depth > 0 then newline else blank)

	DNode tag d
	 -> tag % ": " % newline
		%> pprDocIndentedWithNewLines (depth - 1) d
	  % (if depth > 0 then newline else blank)

	DList ds
	 -> vcat 
		$ map (pprDocIndentedWithNewLines depth) 
		$ filter (not . docIsEmpty) ds
	
	DLeaf str
	 -> str
	
	
-- | Check if a `Doc` is just made of `DBlank`s or empty `DList`s.
docIsEmpty :: Doc str -> Bool
docIsEmpty dd
 = case dd of
	DBlank			-> True

	DNode _ d		-> docIsEmpty d

	DList []		-> True
	DList (dd' : ds)
	 | not $ docIsEmpty dd'	-> False
	 | otherwise		-> docIsEmpty (DList ds)

	DLeaf _			-> False
	
	