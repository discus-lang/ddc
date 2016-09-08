
-- | Attach calling conventions to ICall instructions.
module DDC.Llvm.Transform.Calls
        (attachCallConvs)
where
import DDC.Llvm.Syntax


-- | Attach calling conventions to call instructions.
attachCallConvs :: Module -> Module
attachCallConvs mm
 = let  funcs'  = map (callsFunction mm) $ modFuncs mm
   in   mm { modFuncs = funcs' }


-- | Attach calling conventions to call instructions in a function.
callsFunction :: Module -> Function -> Function
callsFunction mm fun
 = let  blocks' = map (callsBlock mm) $ funBlocks fun
   in   fun  { funBlocks = blocks' }


-- | Attach calling conventions to call instructions in a block.
callsBlock    :: Module -> Block -> Block
callsBlock mm block
 = let  instrs' = fmap (callsInstr mm) $ blockInstrs block
   in   block { blockInstrs = instrs' }


-- | Attach calling conventions to call instructions,
--   leaving other instructions unharmed.
callsInstr    :: Module -> AnnotInstr -> AnnotInstr
callsInstr mm ai@(AnnotInstr i annots)
 = case i of
        ICall mv ct mcc t n xs ats
         | Just cc2     <- callConvOfName mm n
         -> let cc'      = mergeCallConvs mcc cc2
            in  AnnotInstr (ICall mv ct (Just cc') t n xs ats)
                           annots

        _ -> ai


-- | Lookup the calling convention for the given name.
callConvOfName :: Module -> Name -> Maybe CallConv
callConvOfName mm name
        -- Functions defined at top level can have different calling
        -- conventions.
        | NameGlobal str <- name
        , Just cc2       <- lookupCallConv str mm
        = Just cc2

        -- Unknown functions bound to variables are assumed to have
        -- the standard calling convention.
        | NameLocal _    <- name 
        = Just CC_Ccc

        | otherwise      = Nothing


-- | If there is a calling convention attached directly to an ICall
--   instruction then it must match any we get from the environment.
mergeCallConvs :: Maybe CallConv -> CallConv -> CallConv
mergeCallConvs mc cc
 = case mc of
        Nothing         -> cc

        Just cc'        
         | cc == cc'    -> cc
         | otherwise    
         -> error $ unlines
                [ "DDC.LLVM.Transform.Clean"
                , "  Not overriding exising calling convention." ]

