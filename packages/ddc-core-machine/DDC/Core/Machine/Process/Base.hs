
module DDC.Core.Machine.Process.Base
        ( Label         (..)
        , Channel       (..)
        , ChannelType   (..)
        , BlockNext     (..)
        , Block         (..)
        , BlockInfo     (..)
        , Process       (..)
        , Network       (..)
        )
where
import DDC.Core.Machine.Prim
import DDC.Type.Exp
import DDC.Core.Exp

import qualified Data.Map as Map

import DDC.Core.Pretty          ()
import DDC.Data.Pretty

newtype Label
    = Label
    { unLabel :: Int }

newtype Channel
    = Channel
    { unChannel :: Name }

data ChannelType
    = ChannelInput | ChannelOutput | ChannelIgnore

data BlockNext
    = BlockNext
    { bnLabel     :: Label
    , bnArguments :: [Exp () Name]
    }

data Block
    = BlockPull Channel BlockNext
    | BlockPush Channel (Exp () Name) BlockNext
    | BlockDrop Channel BlockNext
    | BlockJump BlockNext
    -- TODO: if/case

data BlockInfo
    = BlockInfo
    { biArguments :: [Bind Name]
    , biBlock     :: Block
    }

data Process
    = Process
    { pInit         :: BlockNext
    , pBlocks       :: Map.Map Label BlockInfo
    -- TODO: this should be computed based on pBlocks
    , pChannelTypes :: Map.Map Channel ChannelType
    }

data Network
    = Network
    { nChannelInputs  :: [Channel]
    , nChannelOutputs :: [Channel]
    , nProcesses      :: [Process]
    }


instance Pretty Label where
 ppr (Label i) = text "l" <> ppr i

instance Pretty Channel where
 ppr (Channel i) = text "c" <> ppr i

instance Pretty ChannelType where
 ppr ChannelInput  = text "Input"
 ppr ChannelOutput = text "Output"
 ppr ChannelIgnore = text "Ignore"

instance Pretty BlockNext where
 ppr (BlockNext lbl args)
  | null args
  = ppr lbl
  | otherwise
  = parens
  $ hcat    $ punctuate space
  $ ppr lbl : map ppr args

instance Pretty Block where
 ppr (BlockPull c n)
  = text "pull#" <+> ppr c <+> ppr n
 ppr (BlockPush c x n)
  = text "push#" <+> ppr c <+> ppr x <+> ppr n
 ppr (BlockDrop c n)
  = text "drop#" <+> ppr c <+> ppr n
 ppr (BlockJump n)
  = ppr n

pprBlockInfoPair :: Label -> BlockInfo -> Doc
pprBlockInfoPair lbl (BlockInfo args block)
 = ppr lbl <+> hcat (punctuate space $ fmap ppr args) <+> text "=" <+> ppr block

pprChannelTypePair :: Channel -> ChannelType -> Doc
pprChannelTypePair c t
 = ppr c <+> text "=" <+> ppr t

instance Pretty Process where
 ppr (Process start blocks chans)
  = vcat
  $  [ text "Process"
     , text " init: " <> indent 2 (ppr start)
     , text " blocks:"]
  ++ map (indent 2 . uncurry pprBlockInfoPair) (Map.toList blocks)
  ++ [ text " channels:" ]
  ++ map (indent 2 . uncurry pprChannelTypePair) (Map.toList chans)

instance Pretty Network where
 ppr (Network ins outs procs)
  = vcat
  $  [ text "Network"
     , text " inputs:  " <> ppr ins
     , text " outputs: " <> ppr outs ]
     ++ map (indent 2 . ppr) procs

