module DDC.Core.Llvm.Convert.Metadata.Tbaa
       ( MDSuper(..)
       , deriveMD 
       , annot 
       , lookup, lookups )
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
import DDC.Core.Llvm.Convert.Metadata.Graph
import qualified DDC.Type.Env       as Env
import qualified DDC.Core.Salt.Name as A
import qualified DDC.Llvm.Instr     as V

import Prelude                      hiding (lookup)
import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Map                     (Map)
import Data.List                    hiding (lookup)
import qualified Data.Map           as Map
import qualified Data.Set           as Set


-- Metadata management --------------------------------------------------------
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


-- | Map region variables to relevant metadata
--      need the whole declaration for tags, e.g "!tbaa", "!debug"
type MDEnv = Map (Bound A.Name) [MDecl]
 
emptyDict :: MDEnv
emptyDict = Map.empty

extendDict :: (Bound A.Name, MDecl) -> MDEnv -> MDEnv
extendDict (u, n) e | Map.member u e = Map.adjust (n:) u e
                    | otherwise      = Map.insert u [n] e

lookup :: Bound A.Name -> MDSuper -> Maybe [MDecl]
lookup u mdsup = Map.lookup u (nameMap mdsup)

lookups :: [Bound A.Name] -> MDSuper -> [Maybe [MDecl]]
lookups us mdsup = map (flip lookup mdsup) us


-- | Generate tbaa metadata for a core (Salt) top-level super
deriveMD
      :: (BindStruct (Exp ()))
      => String                       -- ^ Sanitized name of super
      -> Exp () A.Name                -- ^ Super to derive from
      -> LlvmM (MDSuper)              -- ^ Metadata encoding witness information            
deriveMD nTop xx
  = let 
        regs                 = collectRegsB xx
        (constwits, diswits) = partitionWits $ collectWitsB xx
        arel                 = constructARel   diswits
        domain               = constructANodes regs constwits
        mdDAG                = transOrientation' $ UG (domain, arel)
        mdTrees              = partitionDAG mdDAG    
    in  foldM (buildMDTree nTop) (MDSuper emptyDict []) mdTrees


buildMDTree :: String -> MDSuper -> Tree ANode ->  LlvmM MDSuper
buildMDTree nTop sup tree
 = let tree' = anchor ARoot tree
   in  bfBuild nTop tree' Nothing sup ARoot

bfBuild :: String -> Tree ANode -> Maybe MRef -> MDSuper -> ANode -> LlvmM MDSuper
bfBuild nTop tree parent sup node
 = case parent of
        Nothing        -> do name <- freshRootName nTop
                             bf Nothing $ tbaaRoot name
        Just parentRef -> do name <- freshNodeName nTop (regionU node)
                             bf (Just $ regionU node) $ tbaaNode name parentRef (isConst node)
   where bf u md 
          = do ref          <- liftM MRef $ newUnique
               let sup'     =  declare u ref md sup
               let children =  sources node tree
               foldM (bfBuild nTop tree (Just ref)) sup' children
         declare u r m s 
          = let decl = MDecl r m
            in  case u of Nothing -> s { decls   = decl:(decls s) }
                          Just u' -> s { nameMap = extendDict (u',decl) $ nameMap s
                                       , decls   = decl:(decls s) }


freshNodeName :: String -> Bound A.Name -> LlvmM String
freshNodeName q (UName (A.NameVar n)) = return $ q ++ "_" ++ n
freshNodeName q _                     = liftA (\i -> q ++ "_" ++ (show i)) newUnique

freshRootName :: String -> LlvmM String
freshRootName qualify = liftA (\i -> qualify ++ "_ROOT_" ++ (show i)) newUnique


-- | Attach relevant metadata to instructions
annot :: (BindStruct c, Show (c A.Name))
      => KindEnv A.Name 
      -> MDSuper                        -- ^ Metadata      
      -> [c A.Name]                     -- ^ Things to lookup for MD
      -> V.Instr                        -- ^ Instruction to annotate
      -> V.AnnotInstr
      
annot kenv mdsup xs ins
 = let regions  = concatMap (collectRegsU kenv) xs
       mdecls   = concat $ catMaybes $ lookups regions mdsup
       annotate' ms is 
         = case is of
                V.ILoad{}  -> V.AnnotInstr (is, ms)
                V.IStore{} -> V.AnnotInstr (is, ms)
                _          -> V.AnnotInstr (is, [])              
   in  annotate' mdecls ins


-- Alias relation -------------------------------------------------------------
-- | A node in the alias graphs, representing a region
data ANode  = ANode { regionU :: RegBound
                   , isConst :: Bool }
            | ARoot
              deriving (Show, Eq)


-- | Make nodes from regions
constructANodes :: [RegBound] -> [WitTypedBound] -> [ANode]
constructANodes regs constwits
 = let isConstR r = or $ map (flip isConstWFor r) constwits
       mkANode r  = ANode r (isConstR r)
   in  map mkANode regs


-- | Encode the `alias` relation defined on the set regions
--      * reflexitivity is taken as implicit 
--        (important for generating DAG, and safe since LLVM already assumes reflexitivity)
--      * symmetry is made explicit
--      note that `alias` is non-transitive.
--
constructARel :: [WitTypedBound] -> Rel ANode
constructARel diswits = alias
  where alias n1 n2
          | n1 == n2  = False
          | otherwise = not $ or $ map (flip isDistinctWFor (regionU n1, regionU n2)) diswits


-- Collecting bounds ----------------------------------------------------------
type RegBound      = Bound A.Name
type WitBound      = Bound A.Name
type WitTypedBound = (WitBound, Type A.Name)

isConstW :: WitTypedBound -> Bool
isConstW (_,t)  = isConstWitType t

isConstWFor :: WitTypedBound -> RegBound -> Bool
isConstWFor (_, t) r
  | _ : args <- takeTApps t
  = and [isConstWitType t, elem (TVar r) args]
  | otherwise = False

isDistinctW :: WitTypedBound-> Bool
isDistinctW (_,tw) 
  | tc : _ <- takeTApps tw = isDistinctWitType tc
  | otherwise              = False

isDistinctWFor :: WitTypedBound -> (RegBound, RegBound) -> Bool
isDistinctWFor (_, t) (r1,r2)
  | tc : args <- takeTApps t
  = and [isDistinctWitType tc, (TVar r1) `elem` args, (TVar r2) `elem` args]
  | otherwise = False


-- | Divide a set of witnesses to a set of Const wits and a set of Distinct wits
partitionWits :: [WitTypedBound] -> ([WitTypedBound], [WitTypedBound])
partitionWits ws
  = partition isConstW
  $ filter    (liftA2 (||) isConstW isDistinctW) ws

-- | Collect region bounds
collectRegsU :: (BindStruct c) => KindEnv A.Name -> c A.Name -> [RegBound]
collectRegsU kenv cc
 = let isReg u = case Env.lookup u kenv of
                      Just t | isRegionKind t -> True
                      _                       -> False
   in  filter isReg $ Set.toList (collectBound cc)

-- | Collect region bindings
collectRegsB :: (BindStruct c) => c A.Name -> [RegBound]
collectRegsB cc
 = let isBindReg b 
         = case b of
                BName n t | isRegionKind t -> Just (UName n)
                _                          -> Nothing
       bindRegs = map (isBindReg) $ fst (collectBinds cc)                            
   in  catMaybes bindRegs   
   
-- | Collect witness bindings together with their types (for convinience)
collectWitsB :: (BindStruct c) => c A.Name -> [(WitBound, Type A.Name)]
collectWitsB cc
 = let isBindWit  b 
         = case b of
                BName n t | isWitnessType t -> Just (UName n, t)
                _                           -> Nothing
       
       bindWits  = map (isBindWit) $ snd (collectBinds cc)
   in  catMaybes bindWits

