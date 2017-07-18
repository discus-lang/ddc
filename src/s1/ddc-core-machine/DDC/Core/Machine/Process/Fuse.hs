{-# OPTIONS_GHC -Wwarn #-}
module DDC.Core.Machine.Process.Fuse
        ( fuseNetwork
        , fusePair
        , FuseError (..)
        )
where
import DDC.Core.Machine.Process.Base
import DDC.Core.Machine.Prim
import qualified Data.Map as Map
import DDC.Core.Exp

import Control.Monad (foldM)

import DDC.Core.Pretty          ()
import DDC.Data.Pretty (Pretty(..), ppr, text, (<>), vcat, punctuate, hcat, renderIndent)

data FuseError
 = FuseErrorInternalError String
 | FuseErrorNoSuchBlock Label Label
 | FuseErrorNeither FuseLabel FuseErrorStep FuseErrorStep

instance Pretty FuseError where
 ppr (FuseErrorInternalError str)
  = text ("Internal error: " ++ str)
 ppr (FuseErrorNoSuchBlock lp lq)
  = text "Malformed input process: no such block at label " <> ppr lp <> text " or " <> ppr lq
 ppr (FuseErrorNeither l s1 s2)
  = vcat
  [ text "Neither machine can progress."
  , text "  label:     " <> ppr l
  , text "  process 1: " <> ppr s1
  , text "  process 2: " <> ppr s2
  ]

data FuseErrorStep
 = FuseErrorStep String

instance Pretty FuseErrorStep where
 ppr (FuseErrorStep str)
  = text str

-- | Static input state.
-- "None" is encoded as not in map
data InputState
    = IS'Pending
    | IS'Have
    deriving (Eq, Ord, Show)

instance Pretty InputState where
 ppr IS'Pending = text "pending"
 ppr IS'Have    = text "have"

data FuseLabel
    = FuseLabel Label (Map.Map Channel InputState) Label (Map.Map Channel InputState)
    deriving (Eq, Ord)

instance Pretty FuseLabel where
 ppr (FuseLabel lp sp lq sq)
  = ppr lp <> text "_" <> pprMap sp <> text "_" <> ppr lq <> text "_" <> pprMap sq
  where
   pprMap ss
    = hcat
    $ punctuate (text "_")
    $ map (\(k,v) -> ppr k <> text "_is_" <> ppr v)
    $ Map.toList ss

labelOfFuseLabel :: FuseLabel -> Label
labelOfFuseLabel l
 -- NOTE: the worst thing ever!!! We could probably just add FuseLabel as a constructor to Name.
 = Label $ NameVar $ renderIndent (ppr l)

data ChannelType2
 = In1 | In2 | In1Out1 | Out1
 deriving Eq

-- | This is "channels" in the paper
fuseChannels2 :: Map.Map Channel ChannelType -> Map.Map Channel ChannelType -> Map.Map Channel ChannelType2
fuseChannels2 ls rs
 = let ls' = Map.map inChan ls
       rs' = Map.map inChan rs
   in  Map.unionWith joinChan ls' rs'
 where
  inChan ChannelInput  = In1
  inChan ChannelOutput = Out1

  joinChan In1  In1  = In2
  joinChan In1  Out1 = In1Out1
  joinChan Out1 In1  = In1Out1
  -- NOTE: these should really be caught in an Either
  joinChan Out1 Out1 = error "'impossible': two processes cannot output to the same stream"
  joinChan _    _    = error "'impossible': channel types cannot be merged"

getChannels1 :: Map.Map Channel ChannelType2 -> Map.Map Channel ChannelType
getChannels1
 = Map.map chan1
 where
  chan1 In1     = ChannelInput
  chan1 In2     = ChannelInput
  chan1 In1Out1 = ChannelOutput
  chan1 Out1    = ChannelOutput


fuseNetwork :: Network -> Either FuseError Network
fuseNetwork (Network ins outs [])
 = return $ Network ins outs []
fuseNetwork (Network ins outs (p:procs))
 -- NOTE need to order these properly
 = do   p' <- foldM fusePair p procs
        return $ Network ins outs [p']

-- | Fuse a pair of processes
fusePair :: Process -> Process -> Either FuseError Process
fusePair p q
 = do   blocks <- go Map.empty l0
        let l0' = labelOfFuseLabel l0
        let blocks' = Map.mapKeys labelOfFuseLabel blocks
        return $ Process (BlockNext l0' l0_args) blocks' chan1s
 where
  l0     = FuseLabel (bnLabel $ pInit p) Map.empty (bnLabel $ pInit q) Map.empty
  l0_args = Map.union (bnArguments $ pInit p) (bnArguments $ pInit q)
  chan2s = fuseChannels2 (pChannelTypes p) (pChannelTypes q)
  chan1s = getChannels1 chan2s


  go bs l
   | Just _ <- Map.lookup l bs
   = return bs

   | FuseLabel l_p _ l_q _ <- l
   , Just b_p <- Map.lookup l_p (pBlocks p)
   , Just b_q <- Map.lookup l_q (pBlocks q)
   = do (b,outs) <- tryStepPair chan2s l b_p b_q
        foldM go (Map.insert l b bs) outs

   | FuseLabel l_p _ l_q _ <- l
   = Left $ FuseErrorNoSuchBlock l_p l_q

tryStepPair :: Map.Map Channel ChannelType2 -> FuseLabel -> Block -> Block -> Either FuseError (Block, [FuseLabel])
tryStepPair cs l@(FuseLabel lp sp lq sq) bp0 bq0
 = let try_p = tryStep id        cs lp sp lq sq bp0
       try_q = tryStep flipLabel cs lq sq lp sp bq0
   in case (try_p, try_q) of
        (Right (bp, outp), Right (bq, outq))
         | notPull bp
         -> return (bp, outp)
         | notPull bq
         -> return (bq, outq)
        (Right (bp, outp), _)
         -> return (bp, outp)
        (_, Right (bq, outq))
         -> return (bq, outq)
        (Left e1, Left e2)
         -> Left (FuseErrorNeither l e1 e2)

 where
  notPull (BlockPull{}) = False
  notPull  _            = True

  flipLabel (FuseLabel la sa lb sb) = FuseLabel lb sb la sa


tryStep :: (FuseLabel -> FuseLabel) -> Map.Map Channel ChannelType2
        -> Label -> Map.Map Channel InputState
        -> Label -> Map.Map Channel InputState
        -> Block
        -> Either FuseErrorStep (Block, [FuseLabel])
tryStep flipper cs lp sp lq sq bp = case bp of
   BlockPull c x (BlockNext l u)
    | Just In1 <- Map.lookup c cs
    -> out1 (BlockPull c x) (goto l sp sq) u

    | Just ct <- Map.lookup c cs
    , is_In2_or_In1Out1 ct
    , Just IS'Pending <- Map.lookup c sp
    -> out1  BlockJump
            (goto l (Map.insert c IS'Have sp) sq)
            (Map.insert x (xchanVar c) u)

    | Just In2 <- Map.lookup c cs
    , Nothing <- Map.lookup c sp
    , Nothing <- Map.lookup c sq
    -> out1 (BlockPull c x)
            (goto lp (Map.insert c IS'Pending sp) (Map.insert c IS'Pending sq))
             Map.empty

    | otherwise
    -> Left $ FuseErrorStep "Can't pull"

   BlockDrop c (BlockNext l u)
    | Just In1 <- Map.lookup c cs
    -> out1 (BlockDrop c) (goto l sp sq) u

    | Just In1Out1 <- Map.lookup c cs
    -> out1 BlockJump (goto l (Map.delete c sp) sq) u

    | Just In2 <- Map.lookup c cs
    , Just _haveOrPending <- Map.lookup c sq
    -> out1 BlockJump (goto l (Map.delete c sp) sq) u

    | Just In2 <- Map.lookup c cs
    , Nothing <- Map.lookup c sq
    -> out1 (BlockDrop c) (goto l (Map.delete c sp) sq) u

    | otherwise
    -> Left $ FuseErrorStep "Can't drop"

   BlockPush c e (BlockNext l u)
    | Just Out1 <- Map.lookup c cs
    -> out1 (BlockPush c e) (goto l sp sq) u

    | Just In1Out1 <- Map.lookup c cs
    , Nothing <- Map.lookup c sq
    -> out1 (BlockPush c e)
            (goto l sp (Map.insert c IS'Pending sq))
            (Map.insert (vchanVar c) e u)

    | otherwise
    -> Left $ FuseErrorStep "Can't push"

   BlockJump (BlockNext l u)
    -> out1 BlockJump (goto l sp sq) u


 where
  out1 blocker l u
   = return (blocker (BlockNext (labelOfFuseLabel l) u), [l])

  vchanVar c = Variable $ chanVar c
  xchanVar c = XVar () $ UName $ chanVar c
  chanVar (Channel c) = NameVarMod c "_buf"

  goto l sp' sq'
   = flipper
   $ FuseLabel l sp' lq sq'

  is_In2_or_In1Out1 In2 = True
  is_In2_or_In1Out1 In1Out1 = True
  is_In2_or_In1Out1 _ = False

