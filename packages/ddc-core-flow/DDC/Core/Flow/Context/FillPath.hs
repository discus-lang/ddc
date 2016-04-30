
module DDC.Core.Flow.Context.FillPath
        ( FillMap, FillPath
        , pathsOfFills
        , getAccForPath
        , getAcc
        , isSimple
        , isNone )
where
import DDC.Type.Exp
import DDC.Core.Flow.Prim
import DDC.Core.Flow.Process.Operator
import DDC.Core.Flow.Context.Base

import qualified Data.Map as Map
import Control.Monad


type FillMap = Map.Map Name (FillPath, Type Name)

data FillPath
        = PathNone
        | PathRate      (Type  Name)
        | PathSelect    (Bound Name)
        | PathSegment   (Bound Name)
        | PathAppend     FillPath       FillPath
        deriving (Eq, Show)


pathsOfFills :: Context -> Maybe FillMap
pathsOfFills ctx
 = go ctx Map.empty
 where
  go c@ContextAppend{} _
   = do m1 <- go (contextInner1 c) Map.empty
        m2 <- go (contextInner2 c) Map.empty
        return $ Map.unionWith merge
          (Map.map appl m1)
          (Map.map appr m2)

  go c m
   = do m' <- insertFillsNoDupes (contextOps c) (path c) m
        foldM (flip go) m' (contextInner c)

  appl (p,t)
   = (PathAppend p PathNone, t)
  appr (p,t)
   = (PathAppend PathNone p, t)

  merge (PathAppend PathNone _, t) (PathAppend _ PathNone, _)
   = (PathNone, t)
  merge (PathAppend l _, t) (PathAppend _ r, _)
   = (PathAppend l r, t)
  merge _ _
   = error "ddc-core-flow.pathsOfFills: impossible!"

  path c@ContextRate{} 
   = PathRate $ contextRate c
  path c@ContextSelect{} 
   = PathSelect $ contextFlags c
  path c@ContextSegment{} 
   = PathSegment $ contextLens c
  path ContextAppend{} 
   = PathAppend PathNone PathNone


  insertFillsNoDupes ops p m
   = foldM (insert1 p) m ops

  insert1 p m OpFill{ opTargetVector = UName n
                    , opElemType     = ty }
   = case Map.lookup n m of
     Nothing
      -> Just (Map.insert n (p,ty) m)
     Just _
      -> Nothing
  insert1 _ m _
   = Just m


isPrefixOf :: FillPath -> FillPath -> Bool
isPrefixOf PathNone _
 = True
isPrefixOf (PathAppend h i) (PathAppend j k)
 =  h == j && isPrefixOf i k
 || i == PathNone && k == PathNone && isPrefixOf h j
isPrefixOf a b
 = a == b

isNone :: FillPath -> Bool
isNone PathNone
 = True
isNone (PathAppend i j)
 = isNone i && isNone j
isNone _
 = False

-- A simple fill path has only one place it's filling, and it's just a rate with no select or segment
isSimple :: FillPath -> Bool
isSimple (PathAppend i j)
 =  isSimple i && isNone j
 || isSimple j && isNone i
isSimple (PathRate _)
 = True
isSimple _
 = False



getAccForPath :: FillMap -> FillPath -> Maybe Name
getAccForPath m fp
 = case Map.minViewWithKey $ Map.filter search m of
   Nothing            -> Nothing
   Just ((k,_),_)     -> Just k
 where
  search (fp', _)
   = isPrefixOf fp fp'

-- If acc is Nothing, you can just use the current index ^0
getAcc :: FillMap -> Name -> Maybe Name
getAcc m n
 = case Map.lookup n m of
   Nothing
    -> Nothing -- ???
   Just (fp, _)
    -> if   isSimple fp
       then Nothing
       else getAccForPath m fp

-- I don't think this actually gives us the fewest number of accumulators.
-- Depending on the ordering of the map, maybe we'd have
--
-- [ h = App a None;
--   i = App a None;
--   j = App a b;
--   k = App a c ]
--   
-- here, @h@ and @i@ will be given the same accumulator, but @j@ and @k@ need separate
-- accumulators: three accumulators in total.
--
-- If the order were different, such as
-- [ j = App a b;
--   h = App a None;
--   i = App a None;
--   k = App a c ]
-- then searching for @h@ or @i@ would find @j@, and we would end up with only two accumulators, @j@ and @k@.
--   

