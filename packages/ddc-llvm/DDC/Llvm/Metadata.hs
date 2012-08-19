module DDC.Llvm.Metadata 
        ( Metadata (..)
        , tbaaNode 
        , tbaaRoot 
        , MDecl    (..)
        , MRef     (..)
        , declares, rval )
where  
import DDC.Base.Pretty
import DDC.Llvm.Type


-- Metadata types -------------------------------------------------------------
-- | Different types of metadata used in LLVM IR
--      e.g. 'debug', 'tbaa', 'range', etc.
data Metadata
        -- Metadata used for type-based alias analysis.
        = Tbaa 
        { identifier :: MDPrim               -- a string uniquely indentify this metadata
        , parent     :: (Maybe MRef)         -- the parent node in the metadata tree
        , isConst    :: ConstFlag            -- whether this region is const
        } deriving (Eq, Show)


-- | Pretty the metadata with all of its contents
--      e.g. !{ metadata !"rx", metadata !0, i11}
instance Pretty Metadata where
 ppr mt
  = case mt of
         Tbaa n p c
           -> text " !" 
           <> braces (hcat $ punctuate (comma <> space) [ppr n, ppr p, ppr c])


-- | Maps matadata references to metadata nodes
--      e.g. !2 = !{ metadata "id", !0, !i11}
data MDecl = MDecl MRef Metadata
             deriving Show

data MRef  = MRef Int 
             deriving (Show, Eq)

instance Pretty MDecl where
  ppr (MDecl ref m) = ppr ref <> space <> equals <> space <> text "metadata" <> ppr m

instance Pretty (MRef) where
  ppr (MRef i) = text ("!" ++ show i)

-- | Pretty for metadata refs that act as operands of other metadata
--      apparently "null" is fine.
instance Pretty (Maybe MRef) where
 ppr (Just m) = ppr m
 ppr Nothing  = text "null"


-- Sugar
declares :: [MDecl] -> [MDecl] -> [MDecl]
declares ms mss = ms ++ mss

rval :: MDecl -> Metadata
rval (MDecl _ m) = m


-- Metadata internal-----------------------------------------------------------
-- | Primitive types of LLVM metadata
data MDPrim 
        = MDString String 
        | MDNode [MDNodeElt]     
        deriving (Eq, Show)

instance Pretty MDPrim where
 ppr mp
  = case mp of
         MDString s -> text "!" <> (dquotes $ text s)
         MDNode  es -> text "!" <> braces (ppr es)


-- | Elements of a metadata node
data MDNodeElt = NodeMD Metadata | NodeT Type deriving (Eq, Show)

instance Pretty MDNodeElt where
 ppr elt
  = case elt of
         NodeMD n@(Tbaa _ _ _) -> ppr n
         NodeT  t -> ppr t
       
       
-- | A TBAA metadata field that indicates whether the node refers to a const region
data ConstFlag = ConstFlag Bool deriving (Eq, Show)           
      
instance Pretty ConstFlag where
 ppr (ConstFlag c) = text "i1" <> text (if c then "1" else "0")
 


-- Constructing TBAA metadata ------------------------------------------------- 
-- | Construct a single tbaa node
tbaaNode
      :: String         -- ^ A unique identifier for the node
      -> MRef           -- ^ The parent node
      -> Bool           -- ^ Whether this node represents a const region
      -> Metadata 
tbaaNode n pr c = Tbaa (MDString n) (Just pr) (ConstFlag c)

tbaaRoot :: String -> Metadata
tbaaRoot n = Tbaa (MDString n) Nothing (ConstFlag True)
      
