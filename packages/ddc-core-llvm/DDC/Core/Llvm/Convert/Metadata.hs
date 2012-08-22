module DDC.Core.Llvm.Convert.Metadata
       ( MDSuper(..) 
       , MDEnv  (..)
       , MDecl  (..) 
       , deriveMetadataM 
       , lookup, lookups, annot)
where

import DDC.Llvm.Metadata
import DDC.Core.Llvm.LlvmM
import DDC.Type.Exp
import DDC.Type.Compounds
import DDC.Type.Predicates
import DDC.Type.Collect
import DDC.Core.Exp
import DDC.Base.Pretty              hiding (empty)
import DDC.Type.Env                 (KindEnv)
import qualified DDC.Type.Env       as Env
import qualified DDC.Core.Salt.Name as A
import qualified DDC.Llvm.Instr     as V

import Prelude                      hiding (lookup)
import Data.Map                     (Map)
import Data.Maybe
import Control.Monad                (foldM, replicateM)
import qualified Data.Set           as Set
import qualified Data.Map           as Map


-- | Metadata for a super
data MDSuper
        = MDSuper
          { -- Map bound regions to metadata nodes for attaching metadata 
            --    to relevant instructions.
            nameMap     :: MDEnv
            
            -- Metadata nodes, to be pretty-printed with the module as
            --    declarations. e.g. "1 = !{ metadata "id", !parent, !i11}
          , decls       :: [MDecl]
          } deriving Show

instance Pretty (MDSuper) where
 ppr (MDSuper _ metadata)
  = vcat $ map ppr metadata
    
    
-- Environment ----------------------------------------------------------------
-- | Map region variables to relevant metadata
--      need the whole declaration for tags, e.g "!tbaa", "!debug"
type MDEnv = Map (Bound A.Name) [MDecl]
 
 
emptyDict :: MDEnv
emptyDict = Map.empty


extendDict :: (Bound A.Name, MDecl) -> MDEnv -> MDEnv
extendDict (u, n) e | Map.member u e = Map.adjust (n:) u e
                    | otherwise      = Map.insert u [n] e


extendsDict :: [(Bound A.Name, MDecl)] -> MDEnv -> MDEnv
extendsDict bs e = foldl (flip extendDict) e bs


lookup :: Bound A.Name -> MDSuper -> Maybe [MDecl]
lookup u mdsup = Map.lookup u (nameMap mdsup)


lookups :: [Bound A.Name] -> MDSuper -> [Maybe [MDecl]]
lookups us mdsup = map (flip lookup mdsup) us


-- Deriving metadata ----------------------------------------------------------
-- | Generate tbaa metadata for a core (Salt) top-level super
deriveMetadataM
            :: (BindStruct (Exp ()))
            => String                       -- ^ Sanitized name of super
            -> Exp () A.Name                -- ^ Super to derive from
            -> LlvmM (MDSuper)              -- ^ Metadata encoding witness information            
deriveMetadataM nTop xx
 = do       
      -- Collect list of distinct pairs and const regions
      let regs      =  collectRegionsB xx
      let wits      =  collectWitnessesB xx
      let consts    =  filter (isConstReg wits) regs
      let distincts =  distinctPairs wits
      
      -- Generate a single-level tree for each distinct pair, using unique names for nodes   
      ms              <- foldM (deriveDistinctM nTop consts) (emptyDict, []) distincts   
      
      -- Generate a node for constant regions
      (mdenv, mdecls) <- foldM (deriveConstM nTop distincts) ms consts
          
      return $ MDSuper mdenv mdecls


qualify :: String -> Bound A.Name -> Int -> String
qualify q (UName (A.NameVar n)) _ = q ++ "_" ++ n
qualify q _                     i = q ++ "_" ++ (show i)

qualifyRoot :: String -> Int -> String
qualifyRoot q i = q ++ "_ROOT_" ++ (show i)


-- | Generate metadata for each pair of distinct regions
deriveDistinctM 
        :: String                        -- ^ Qualify all metadata names with this
        -> [Bound A.Name]                -- ^ Constant regions
        -> (MDEnv, [MDecl])              -- ^ Accumulator
        -> (Bound A.Name, Bound A.Name)  -- ^ Distinct pair
        -> LlvmM (MDEnv, [MDecl])       
deriveDistinctM fn consts (mdenv, mdecls) (r1, r2)
  = do   [nr,n1,n2]     <- replicateM 3 newUnique
         let root       =  tbaaRoot $ qualifyRoot fn nr
         let mkTbaa u n =  tbaaNode (qualify fn u n) (MRef nr) (u `elem` consts)
         let (dr,d1,d2) = ( MDecl (MRef nr)   root
                          , MDecl (MRef n1) $ mkTbaa r1 n1
                          , MDecl (MRef n2) $ mkTbaa r2 n2)
         return $ ( extendsDict (zip [r1, r2] [d1, d2]) mdenv
                  , declares    [dr, d1, d2]            mdecls )
  


-- | Generate metadata for constant regions that are not already included in some
--      distinct pair.                  
deriveConstM
        :: String                          -- ^ Qualify metadata identifiers with this
        -> [(Bound A.Name, Bound A.Name)]  -- ^ Distinct pairs
        -> (MDEnv, [MDecl])                -- ^ Accumulator
        -> Bound A.Name                    -- ^ Constant region to derive metadata for
        -> LlvmM (MDEnv, [MDecl])
deriveConstM fn distincts (mdenv, mdecls) c
  | (ds1, ds2) <- unzip distincts
  , (c `elem` ds1) || (c `elem` ds2)
  = return $ (mdenv, mdecls)
  | otherwise
  = do   [nr, n]    <- replicateM 2 newUnique
         let root   =  tbaaRoot $ qualifyRoot fn nr
         let (dr,d) =  ( MDecl (MRef nr)  root
                       , MDecl (MRef n) $ tbaaNode (qualify fn c n) (MRef nr) True)
         return $ ( extendDict (c, d)   mdenv
                  , declares   [dr, d]  mdecls )
      

-- | Convert distinct witnesses to tuples of distinct regions                                    
distinctPairs 
          :: [(Bound A.Name, Type A.Name)]   -- ^ Witnesses 
          -> [(Bound A.Name, Bound A.Name)]  -- ^ Pairs of regions that have a distinct witness
distinctPairs ws
 = let toDistinctPair (_,tw) | tc : [TVar u1, TVar u2] <- takeTApps tw
                             , isDistinctWitType tc
                             = Just (u1, u2)
                             | otherwise = Nothing      
   in catMaybes $ map (toDistinctPair) ws
     
   
-- | Check if a region has a witness for constancy
isConstReg :: [(Bound A.Name, Type A.Name)] -> Bound A.Name -> Bool
isConstReg ws r
 = let  isConstWit (_, t) | isConstWitType t
                          , _ : args <- takeTApps t
                          , elem (TVar r) args
                          = True
                          | otherwise = False 
   in   if   null $ filter isConstWit ws
        then False else True
  
  
-- Attaching metadata ---------------------------------------------------------  
-- | Attach relevant metadata to instructions
annot :: (BindStruct c, Show (c A.Name))
      => KindEnv A.Name 
      -> MDSuper                        -- ^ Metadata available      
      -> [c A.Name]                     -- ^ Things to lookup for MD
      -> V.Instr                        -- ^ Instruction to annotate
      -> V.AnnotInstr
      
annot kenv mdsup xs ins
 = let regions  = concatMap (collectRegionsU kenv) xs
       mdecls   = concat $ catMaybes $ lookups regions mdsup
       annotate' ms is 
         = case is of
                V.ILoad{}  -> V.AnnotInstr (is, ms)
                V.IStore{} -> V.AnnotInstr (is, ms)
                _          -> V.AnnotInstr (is, [])              
   in  annotate' mdecls ins

 
-- Collecting bounds ----------------------------------------------------------  
-- | Collect region bindings
collectRegionsB 
          :: (BindStruct c) 
          => c A.Name 
          -> [Bound A.Name]
collectRegionsB cc
 = let isBindReg b 
         = case b of
                BName n t | isRegionKind t -> Just (UName n)
                _                          -> Nothing
       bindRegs = map (isBindReg) $ fst (collectBinds cc)                            
   in  catMaybes bindRegs
 

-- | Collect region bounds
collectRegionsU
          :: (BindStruct c)
          => KindEnv A.Name
          -> c A.Name
          -> [Bound A.Name]
collectRegionsU kenv cc
 = let isReg u = case Env.lookup u kenv of
                      Just t | isRegionKind t -> True
                      _                       -> False
   in  filter isReg $ Set.toList (collectBound cc)
   
   
-- | Collect witness bindings together with their types (for convinience)
collectWitnessesB 
          :: (BindStruct c) 
          => c A.Name
          -> [(Bound A.Name, Type A.Name)]
collectWitnessesB cc
 = let 
       isBindWit  b 
         = case b of
                BName n t | isWitnessType t -> Just (UName n, t)
                _                           -> Nothing
       
       bindWits  = map (isBindWit) $ snd (collectBinds cc)
   in  catMaybes bindWits

