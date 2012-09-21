module DDC.Llvm.Metadata 
        ( Metadata (..)
        , tbaaNode 
        , tbaaRoot 
        , MDecl    (..)
        , MRef     (..)
        , rval )
where  
import DDC.Base.Pretty
import DDC.Llvm.Type


-- Metadata types -------------------------------------------------------------
-- | Different types of metadata used in LLVM IR
--      e.g. 'debug', 'tbaa', 'range', etc.
data Metadata
        -- Metadata used for type-based alias analysis.
        = Tbaa  MDNode
        -- Metadata for debugging, here as an example only.
        | Debug
        deriving (Eq, Show)
        
instance Pretty Metadata where
 ppr mt
  = case mt of
         Tbaa (MDNode ops) -> text "!" <> encloseSep lbrace rbrace (comma <> space) (map ppr ops)
         Debug             -> text "DEBUGMD"


-- | Maps matadata references to metadata nodes
--      e.g. !2 = !{ metadata "id", !0, !i11}
data MDecl = MDecl MRef Metadata
             deriving Show

instance Pretty MDecl where
  ppr (MDecl ref m) =  ppr ref 
                    <> space <> equals <> space 
                    <> text "metadata" <> space
                    <> ppr m


data MRef = MRef Int 
            deriving (Show, Eq)

instance Pretty (MRef) where
  ppr (MRef i) = text ("!" ++ show i)

rval :: MDecl -> Metadata
rval (MDecl _ m) = m


-- Metadata internal-----------------------------------------------------------
-- | Primitive types of LLVM metadata
data MDString = MDString String   
                deriving (Eq, Show)
                
instance Pretty MDString where
  ppr (MDString s) = text "!" <> (dquotes $ text s)
  
  
data MDNode = MDNode   [MDNodeOp]     
              deriving (Eq, Show)
                
instance Pretty MDNode where
  ppr (MDNode ns) = text "!" <> braces (ppr ns)


-- Operands to metadata nodes
--    Note: no type parameter to avoid using existentials
data MDNodeOp = OpNull
              | OpMDString  MDString
              | OpMDNode    MDNode
              | OpMDRef     MRef
              | OpBool      Bool
              | OpType      Type
              deriving (Eq, Show)              
              
instance Pretty MDNodeOp where
 ppr elt
  = case elt of
         OpNull        -> text "null"
         OpMDString ms -> text "metadata" <> space <> ppr ms
         OpMDNode   ns -> text "metadata" <> space <> ppr ns
         OpMDRef    r  -> text "metadata" <> space <> ppr r 
         OpBool     b  -> text "i32"      <> space <> text (if b then "1" else "0")
         OpType     t  -> ppr t 


-- TBAA metadata -------------------------------------------------------------- 
-- | Construct a single tbaa node
tbaaNode
      :: String         -- ^ A unique identifier for the node
      -> MRef           -- ^ The parent node
      -> Bool           -- ^ Whether this node represents a const region
      -> Metadata 
tbaaNode n pr c = Tbaa $ MDNode [ OpMDString (MDString n)
                                , OpMDRef     pr
                                , OpBool      c ]

tbaaRoot :: String -> Metadata
tbaaRoot n = Tbaa $ MDNode [ OpMDString (MDString n)
                           , OpNull
                           , OpBool     True ]
                           
