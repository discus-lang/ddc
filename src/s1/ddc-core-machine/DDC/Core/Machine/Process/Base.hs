
module DDC.Core.Machine.Process.Base
        ( Label         (..)
        , Channel       (..)
        , Variable      (..)
        , ChannelType   (..)
        , BlockNext     (..)
        , Block         (..)
        , Process       (..)
        , Network       (..)
        )
where
import DDC.Core.Machine.Prim
import DDC.Core.Exp

import qualified Data.Map as Map

import DDC.Core.Pretty          ()
import DDC.Data.Pretty

newtype Label
    = Label
    { unLabel :: Name }
    deriving (Eq, Ord, Show)

newtype Channel
    = Channel
    { unChannel :: Name }
    deriving (Eq, Ord, Show)

newtype Variable
    = Variable
    { unVariable :: Name }
    deriving (Eq, Ord, Show)

data ChannelType
    = ChannelInput | ChannelOutput

data BlockNext
    = BlockNext
    { bnLabel     :: Label
    , bnArguments :: Map.Map Variable (Exp () Name)
    }

data Block
    = BlockPull Channel Variable BlockNext
    | BlockPush Channel (Exp () Name) BlockNext
    | BlockDrop Channel BlockNext
    | BlockJump BlockNext


data Process
    = Process
    { pInit         :: BlockNext
    , pBlocks       :: Map.Map Label Block
    , pChannelTypes :: Map.Map Channel ChannelType
    }

data Network
    = Network
    { nChannelInputs  :: [Channel]
    , nChannelOutputs :: [Channel]
    , nProcesses      :: [Process]
    }


instance Pretty Label where
 ppr (Label i) = ppr i

instance Pretty Channel where
 ppr (Channel i) = ppr i

instance Pretty Variable where
 ppr (Variable i) = ppr i

instance Pretty ChannelType where
 ppr ChannelInput  = text "Input"
 ppr ChannelOutput = text "Output"

instance Pretty BlockNext where
 ppr (BlockNext lbl args)
  | null args
  = ppr lbl
  | otherwise
  = parens
  $ hcat    $ punctuate space
  $ ppr lbl : map pprPair (Map.toList args)
  where
   pprPair (k,v) = text "{" <> ppr k <> text "=" <> ppr v <> text "}"

instance Pretty Block where
 ppr (BlockPull c v n)
  = text "pull#" <+> ppr c <+> ppr v <+> ppr n
 ppr (BlockPush c x n)
  = text "push#" <+> ppr c <+> pprPrec 11 x <+> ppr n
 ppr (BlockDrop c n)
  = text "drop#" <+> ppr c <+> ppr n
 ppr (BlockJump n)
  = ppr n

pprBlockPair :: Label -> Block -> Doc
pprBlockPair lbl block
 = ppr lbl <+> text "=" <+> ppr block

pprChannelTypePair :: Channel -> ChannelType -> Doc
pprChannelTypePair c t
 = ppr c <+> text "=" <+> ppr t

instance Pretty Process where
 ppr (Process start blocks chans)
  = vcat
  $  [ text "Process"
     , text " init: " <> indent 2 (ppr start)
     , text " blocks:"]
  ++ map (indent 2 . uncurry pprBlockPair) (Map.toList blocks)
  ++ [ text " channels:" ]
  ++ map (indent 2 . uncurry pprChannelTypePair) (Map.toList chans)

instance Pretty Network where
 ppr (Network ins outs procs)
  = vcat
  $  [ text "Network"
     , text " inputs:  " <> ppr ins
     , text " outputs: " <> ppr outs ]
     ++ map (indent 2 . ppr) procs

