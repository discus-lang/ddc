
-- | Abstract block structured documents.
--	Whereas a str is just a flat slab of text, a `Doc` contains some
--	internal structure that we can write to file then load back in.
--
module DDC.Util.Doc
	( Doc(..)
	, Docable(..) 
	, pprDocIndentedWithNewLines )
where
import DDC.Util.Pretty
import Data.Set			(Set)
import qualified Data.Set	as Set


-- | An abstract block structured document.
data Doc str
	= -- | An empty document, with nothing in it.
	  DBlank
	
	-- | A document node \/ section.
	| DNode
		String		-- ^ node name
		(Doc str)	-- ^ node contents

	-- | A compund document.
	| DList [Doc str]

	-- | String data.
	| DLeaf str
	deriving Show


-- | Something we can produce a document of.
class Docable a str | a -> str where
	doc 	:: a -> Doc str

instance Docable a str => Docable (Set a) str 
 where	doc x	= DList $ map doc $ Set.toList x

instance Docable a str => Docable [a] str
 where	doc xx	= DList $ map doc xx


pprDocIndentedWithNewLines
	:: Set String			-- ^ tag of nodes to put extra new lines after
	-> Doc (PrettyM mode) 
	-> PrettyM mode

pprDocIndentedWithNewLines tagNew dd
 = case dd of
	DBlank
	 -> blank

	DNode tag d@(DLeaf{})
	 | length tag <= 6
	 -> padL 7 (tag ++ ":") %> (" " % pprDocIndentedWithNewLines tagNew d)
	  % (if Set.member tag tagNew then newline else blank)

	DNode tag d
	 -> tag % ": " % newline
		%> pprDocIndentedWithNewLines tagNew d
	  % (if Set.member tag tagNew then newline else blank)

	DList ds
	 -> vcat $ map (pprDocIndentedWithNewLines tagNew) ds
	
	DLeaf str
	 -> str
	






