
module DDC.Core.Tetra.Transform.Curry
        (curryModule)
where
import DDC.Type.Env                             (KindEnv, TypeEnv)
import DDC.Core.Transform.TransformDownX
import DDC.Core.Tetra
import DDC.Core.Module
import DDC.Core.Exp
import DDC.Core.Compounds
import Data.Map                                 (Map)
import qualified DDC.Type.Env                   as Env
import qualified Data.Map                       as Map


-- | Insert primitives to manage higher order functions in a module.
curryModule 
        :: Show a
        => Module a Name -> Module a Name
curryModule mm
 = let
        topTypes        = moduleTopBindTypes mm

        xBody'          = transformDownX (curryX topTypes)
                                Env.empty Env.empty 
                        $ moduleBody mm

   in   mm { moduleBody = xBody' }

-- TODO: need to rewrite types on lambda bindings as we decend into the tree.
-- \(f : a -> b)  becomes  \(f : C# (a -> b))


curryX  :: (Show a)
        => Map Name (Type Name)
        -> KindEnv Name -> TypeEnv Name
        -> Exp a   Name -> Exp a   Name

curryX topTypes _kenv _tenv xx 
 | Just (xF, xsArgs)  <- takeXApps xx
 , XVar a (UName nF)  <- xF
 , length xsArgs  > 0
 = makeCall a topTypes nF xsArgs

 | otherwise
 = xx


-- | Call a thing, depending on what it is.
makeCall
        :: (Show a)
        => a 
        -> Map Name (Type Name)
        -> Name  -> [Exp a Name]
        -> Exp a Name

makeCall a topTypes nF xsArgs
 | Just _       <- Map.lookup nF topTypes
 = let  iArgs   = length xsArgs
        iArity  = iArgs
   in   makeCallSuper a nF iArity xsArgs

 | otherwise
 = makeCallThunk a nF xsArgs



-- | Call a top-level supercombinator.
makeCallSuper 
        :: a
        -> Name -> Int -> [Exp a Name]
        -> Exp a Name

makeCallSuper a nF _iArity xsArgs
 = xApps a (XVar a (UName nF)) xsArgs


-- | Call a thunk.
makeCallThunk
        :: Show a
        => a
        -> Name -> [Exp a Name]
        -> Exp a Name

makeCallThunk a nF xsArgs
 = xApps a (XVar a (UName nF)) xsArgs
 -- error $ "call thunk " ++ show (nF, xsArgs)





