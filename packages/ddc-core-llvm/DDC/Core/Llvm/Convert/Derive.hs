module DDC.Core.Llvm.Convert.Derive
       ( MDSuper(..) 
       , MDEnv  (..)
       , MDecl  (..) 
       , deriveMetadataM 
       , lookup, lookups )
where

import DDC.Llvm.Metadata
import DDC.Core.Llvm.LlvmM
import DDC.Type.Exp
import DDC.Type.Compounds
import DDC.Type.Predicates
import DDC.Type.Env                 (Env)
import DDC.Type.Collect
import DDC.Core.Exp
import DDC.Base.Pretty              hiding (empty)
import DDC.Type.Env                 (Env)
import qualified DDC.Type.Env       as Env
import qualified DDC.Core.Salt.Name as A

import Prelude                      hiding (lookup)
import Data.Map                     (Map)
import Data.Maybe
import Control.Monad                (foldM, replicateM)
import qualified Data.Map           as Map
import qualified Data.Set           as Set


-- | Metadata for a super
data MDSuper
        = MDSuper
          { -- Map bound regions to metadata nodes for attaching metadata 
            --    to relevant instructions.
            nameMap     :: MDEnv
            
            -- Metadata nodes, to be pretty-printed with the module as
            --    declarations. e.g. "1 = !{ metadata "id", !parent, !i11}
          , decls       :: [MDecl]
          }

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
            :: (BindStruct (Module ()), Ord n, Show n)
            => Env n                   -- ^ Kind environment
            -> Env n                   -- ^ Type environment
            -> Module () n             -- ^ Module to derive from
            -> LlvmM (MetadataSet n)   -- ^ Metadata encoding witness information            
deriveMetadataM kenv tenv mm
 = do 
      -- Add all the binds this module to the environment, so we have all the region
      --    variables and witnesses in scope
      let bs    =  collectBinds xx
      let kenv' =  Env.extends (fst bs) kenv
      let tenv' =  Env.extends (snd bs) tenv
       
      -- Collect list of distinct pairs and const regions
      let regs      =  collectRegions kenv' xx
      let wits      =  collectWitnesses tenv' xx
      let consts    =  filter (isConstReg wits) regs
      let distincts =  distinctPairs wits
            
      -- Generate a single-level tree for each distinct pair, using unique names for nodes   
      ms              <- foldM (deriveDistinctM nTop consts) (emptyDict, []) distincts   
      
      -- Generate a node for constant regions
      (mdenv, mdecls) <- foldM (deriveConstM nTop) ms consts
          
      return $ MDSuper mdenv mdecls


qualify :: String -> Bound A.Name -> Int -> String
qualify q (UName (A.NameVar n)) _ = q ++ "_" ++ n
qualify q _                     i = q ++ "_" ++ (show i)

qualifyRoot :: String -> Int -> String
qualifyRoot q i = q ++ "_ROOT_" ++ (show i)

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
          :: [(Bound A.Name, Type A.Name)]   -- ^ Witnesses 
          -> [(Bound A.Name, Bound A.Name)]  -- ^ Pairs of regions that have a distinct witness
distinctPairs ws
 = let toDistinctPair (_,tw) | tc : [TVar u1, TVar u2] <- takeTApps tw
                             , isDistinctWitType tc
                             = Just (u1, u2)
                             | otherwise = Nothing      
   in catMaybes $ map (toDistinctPair) ws
     
   
-- | Check in a region has a witness for constancy
isConstReg :: [(Bound A.Name, Type A.Name)] -> Bound A.Name -> Bool
isConstReg ws r
 = let  isConstWit (_, t) | tc : args <- takeTApps t
                          , isConstWitType tc
                          , elem (TVar r) args
                          = True
                          | otherwise = False 
   in   if   null $ filter isConstWit ws
        then False else True
  
  
-- Attaching metadata ---------------------------------------------------------  
-- | Attach relevant metadata to instructions
{-annotate
      :: (BindStruct c)
      => Env A.Name                     -- ^ Kind environment
      -> MDSuper                          -- ^ Metadata available      
      -> [c A.Name ]                    -- ^ Arguments to the primop
      -> Instr                          -- ^ Instruction to annotate
      -> InstrMD
      
annotate kenv MDSuper xxs ins
 = let regions  = concatMap (collectRegions kenv) xxs
       mdecls   = concat $ catMaybes $ lookups regions MDSuper
       mds      = map rval mdecls
       annotate' ms is 
         = case is of
                ILoad{}  -> InstrMD is ms
                IStore{} -> InstrMD is ms
                _        -> InstrMD is []              
   in  annotate' mds ins


annotateSeq
        :: (BindStruct c)
        => Env A.Name -> MDSuper -> [c A.Name ]
        -> Seq Instr -> Seq InstrMD

annotateSeq kenv MDSuper xxs = fmap (annotate kenv MDSuper xxs)-}
 
 
-- Collecting bounds ----------------------------------------------------------  
-- | Collect region variables
collectRegions 
          :: (BindStruct c) 
          => Env A.Name 
          -> c A.Name 
          -> [Bound A.Name]
collectRegions kenv cc
 = let regionKind u = case Env.lookup u kenv of
                             Just t | isRegionKind t -> True
                             _                       -> False
   in filter regionKind $ Set.toList (collectBound cc)
 
 
-- | Collect witness terms together with their types (for convinience)
collectWitnesses 
          :: (BindStruct c) 
          => Env A.Name 
          -> c A.Name
          -> [(Bound A.Name, Type A.Name)]
collectWitnesses tenv cc
 = let witnessType u = case Env.lookup u tenv of
                            Just t | isWitnessType t -> Just (u, t)
                            _                        -> Nothing
   in  catMaybes $ map (witnessType) $ Set.toList (collectBound cc)



