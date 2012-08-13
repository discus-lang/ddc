module DDC.Core.Llvm.Convert.Derive
       ( deriveMetadataM 
       , MetadataSet(..) 
       , lookup, lookups )
where

import DDC.Llvm.Metadata
import DDC.Core.Llvm.LlvmM
import DDC.Type.Exp
import DDC.Type.Compounds
import DDC.Type.Predicates
import DDC.Type.Env                 (Env)
import DDC.Type.Collect
import DDC.Core.Module
import DDC.Base.Pretty              hiding (empty)
import qualified DDC.Type.Env       as Env

import Prelude                      hiding (lookup)
import Data.Map                     (Map)
import Data.Maybe
import Control.Monad                (foldM, replicateM)
import qualified Data.Map           as Map
import qualified Data.Set           as Set


-- | Tbaa metadata set for a whole module
data MetadataSet n
        = MetadataSet
          { -- Environment of metadata names (e.g. !0, !2) and bound names (e.g "r1")
            --    for attaching metadata to instructions.
            nameEnv  :: MetadataEnv n

            -- Metadata "items", to be pretty-printed with the module
          , metadata :: [Metadata]

          } deriving (Show)

instance Pretty (MetadataSet n) where
 ppr (MetadataSet _ metadata)
  = vcat $ map ppr metadata


-- Environment ----------------------------------------------------------------
type MetadataEnv n = Map (Bound n) [Name]

empty :: MetadataEnv n
empty = Map.empty

extend :: (Ord n, Show n) => (Bound n, Name) -> MetadataEnv n -> MetadataEnv n
extend (u, n) e | Map.member u e = Map.adjust (n:) u e
                | otherwise      = Map.insert u [n] e

extends :: (Ord n, Show n) => [(Bound n, Name)] -> MetadataEnv n -> MetadataEnv n
extends bs e = foldl (flip extend) e bs

lookup :: (Ord n, Show n) => Bound n -> MetadataEnv n -> Maybe [Name]
lookup = Map.lookup

lookups :: (Ord n, Show n) => [Bound n] -> MetadataEnv n -> [Maybe [Name]]
lookups us menv = map (flip lookup menv) us


-- Deriving metadata ----------------------------------------------------------
deriveMetadataM
            :: (BindStruct (Module ()), Ord n, Show n)
            => Env n                   -- ^ Kind environment
            -> Env n                   -- ^ Type environment
            -> Module () n             -- ^ Module to derive from
            -> LlvmM (MetadataSet n)   -- ^ Metadata encoding witness information            
deriveMetadataM kenv tenv mm
 = do 
      -- Add all the binds this module to the environment, so we have all the region
      --    variables and witnesses in scope
      let bs    =  collectBinds mm
      let kenv' =  Env.extends (fst bs) kenv
      let tenv' =  Env.extends (snd bs) tenv
      
      -- Collect list of distinct pairs and const regions
      let regs      =  collectRegions kenv' mm
      let wits      =  collectWitnesses tenv' mm
      let consts    =  filter (isConstReg wits) regs
      let distincts =  distinctPairs wits
      
      -- Generate a single-level tree for each distinct pair, using unique names for nodes   
      ms               <- foldM (deriveDistinctM consts) (empty, []) distincts   
      
      -- Generate a node for constant regions
      (menv, metadata) <- foldM (deriveConstM) ms consts
          
      return $ MetadataSet menv metadata

deriveDistinctM 
        :: (Ord n, Show n)
        => [Bound n]
        -> (MetadataEnv n, [Metadata])
        -> (Bound n, Bound n) 
        -> LlvmM (MetadataEnv n, [Metadata])
deriveDistinctM consts (menv, ms) (r1, r2)
  = do   ns               <- replicateM 3 newUnique
         let [n1, n2, rn] = map show ns
         let root     = tbaaRoot rn
         let mkTbaa n = tbaaNamedNode n root (r1 `elem` consts)
         return $ ( extends (zip [r1,r2] [Name n1, Name n2]) menv
                  , [ root, mkTbaa n1, mkTbaa n2 ] ++ ms )      
      
deriveConstM
        :: (Ord n, Show n)
        => (MetadataEnv n, [Metadata])
        -> Bound n
        -> LlvmM (MetadataEnv n, [Metadata])
deriveConstM (menv, ms) c
  = do   ns <- replicateM 2 newUnique
         let [n, rn] = map show ns
         let root    = tbaaRoot rn
         return $ ( extend (c, Name n) menv
                  , [ root, tbaaNamedNode n root True ] ++ ms)
                              
distinctPairs 
          :: (Ord n, Show n)
          => [(Bound n, Type n)]   -- ^ Witnesses 
          -> [(Bound n, Bound n)]  -- ^ Pairs of regions with a distinct witness
distinctPairs ws
 = let toDistinctPair (_,tw) | tc : [TVar u1, TVar u2] <- takeTApps tw
                             , isDistinctWitType tc
                             = Just (u1, u2)
                             | otherwise = Nothing      
   in map fromJust $ filter isJust $ map (toDistinctPair) ws
   

isConstReg :: (Ord n, Show n) => [(Bound n, Type n)] -> Bound n -> Bool
isConstReg ws r
 = let  isConstWit (_, t) | tc : args <- takeTApps t
                          , isConstWitType tc
                          , elem (TVar r) args
                          = True
                          | otherwise = False 
   in   if   null $ filter isConstWit ws
        then False else True
  

collectRegions :: (BindStruct (Module ()), Ord n, Show n) => Env n -> Module () n -> [Bound n]
collectRegions kenv mm
 = let regionKind u = case Env.lookup u kenv of
                             Just t | isRegionKind t -> True
                             _                       -> False
   in filter regionKind $ Set.toList (collectBound mm)
 
 
collectWitnesses :: (BindStruct (Module ()), Ord n, Show n) => Env n -> Module () n -> [(Bound n, Type n)]
collectWitnesses tenv mm
 = let witnessType u = case Env.lookup u tenv of
                            Just t | isWitnessType t -> Just (u, t)
                            _                        -> Nothing
   in  map fromJust $ filter isJust $ map (witnessType) $ Set.toList (collectBound mm)

