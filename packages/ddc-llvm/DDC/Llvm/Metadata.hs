module DDC.Llvm.Metadata 
        ( Metadata (..)
        , Name (..)
        , tbaaNamedNode 
        , tbaaRoot )
where
  
import DDC.Base.Pretty
import DDC.Llvm.Type


-- | Name of a named metadata
data Name = Name String deriving (Eq, Show, Ord)

instance Pretty Name where
 ppr (Name s) = text ("!" ++ s)

instance Pretty (Maybe Name) where
 ppr (Just n) = ppr n
 ppr Nothing  = text "null"


-- | Different types of metadata used in LLVM IR, e.g. 'debug', 'tbaa', 'range', etc.
data Metadata
        -- Metadata used for type-based alias analysis.
        = Tbaa 
        { ref        :: (Maybe Name)         -- the name if this metadat is named
        , identifier :: MetaPrim             -- a string uniquely indentify the region
        , parent     :: (Maybe Name)         -- the parent node in the metadata tree
        , isConst    :: ConstFlag            -- whether this region is const
        } deriving (Eq, Show)

instance Pretty Metadata where
 ppr mt
  = case mt of
         Tbaa (Just r) n p c
           -> ppr r 
           <> text " = metadata !" 
           <> braces (hcat $ punctuate (comma <> space) [ppr n, ppr p, ppr c])
         Tbaa (Nothing) n p c
           -> text "metadata !"
           <> braces (hcat $ punctuate (comma <> space) [ppr n, ppr p, ppr c])


-- | Primitive types of LLVM metadata
data MetaPrim 
        = MetaString String 
        | MetaNode [MetaNodeElt]     
        deriving (Eq, Show)

instance Pretty MetaPrim where
 ppr mp
  = case mp of
         MetaString s -> text "metadata !"  <> (dquotes $ text s)
         MetaNode  es -> text "metadata !{" <> ppr es <> text "}"


-- | Elements of a metadata node
data MetaNodeElt = NodeMD Metadata | NodeT Type deriving (Eq, Show)

instance Pretty MetaNodeElt where
 ppr elt
  = case elt of
         NodeMD (Tbaa (Just n) _ _ _) -> ppr n
         NodeMD (Tbaa Nothing _ _ _)  -> text "foo"
         NodeT  t -> ppr t
       
       
-- | A TBAA metadata field that indicates whether the node refers to a const region
data ConstFlag = ConstFlag Bool deriving (Eq, Show)           
      
instance Pretty ConstFlag where
 ppr (ConstFlag c) = text "i1" <> text (if c then "1" else "0")
 

-- Constructing TBAA metadata ------------------------------------------------- 

-- | Construct a single named tbaa metadata node, using the same name
--      for the reference and the ID.
tbaaNamedNode
      :: String         -- ^ A unique name for the node
      -> Metadata       -- ^ The parent node
      -> Bool           -- ^ Whether this node represents a const region
      -> Metadata 
tbaaNamedNode n p c
 = case p of
        Tbaa (Just pn) _ _ _ -> Tbaa (Just $ Name n) (MetaString n) (Just pn) (ConstFlag c)
        _                    -> error $ "Parent of a tbaa node must be another named tbaa node."

tbaaRoot :: String -> Metadata
tbaaRoot n = Tbaa (Just $ Name n) (MetaString n) Nothing (ConstFlag True)
      
