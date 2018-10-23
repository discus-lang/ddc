
module DDC.Core.Salt.Transform.Slotify.Object
        (objectsOfExp)
where
import DDC.Core.Exp
import qualified DDC.Core.Salt                  as A
import qualified DDC.Core.Salt.Compounds        as A
import Data.Map                 (Map)
import qualified Data.Map       as Map


-- Exp --------------------------------------------------------------------------------------------
objectsOfExp
        :: Exp a A.Name
        -> Map A.Name (Type A.Name)

objectsOfExp xx
 = case xx of
        XVar  _ _       -> Map.empty

        XAbs  _ (MType _) x
         -> objectsOfExp x

        XAbs  _ (MTerm b) x
         -> Map.union  (objectsOfBind b)   (objectsOfExp x)

        XAbs  _ (MImplicit b) x
         -> Map.union  (objectsOfBind b)   (objectsOfExp x)

        XApp  _ x1 x2   -> Map.union  (objectsOfExp x1)   (objectsOfArg x2)
        XLet  _ lts x   -> Map.union  (objectsOfLets lts) (objectsOfExp x)

        XAtom{}         -> Map.empty
        XCase _ x alts  -> Map.unions (objectsOfExp x : map objectsOfAlt alts)
        XCast _ _ x     -> objectsOfExp x


-- Arg --------------------------------------------------------------------------------------------
objectsOfArg :: Arg a A.Name -> Map A.Name (Type A.Name)
objectsOfArg aa
 = case aa of
        RType{}         -> Map.empty
        RWitness{}      -> Map.empty
        RTerm x         -> objectsOfExp x
        RImplicit arg'  -> objectsOfArg arg'


-- Let --------------------------------------------------------------------------------------------
objectsOfLets
        :: Lets a A.Name
        -> Map A.Name (Type A.Name)

objectsOfLets lts
 = case lts of
        LLet b x        -> Map.union (objectsOfBind b) (objectsOfExp x)
        LRec bxs        -> Map.unions [Map.union (objectsOfBind b) (objectsOfExp x) | (b, x) <- bxs]
        LPrivate{}      -> Map.empty


-- Alt --------------------------------------------------------------------------------------------
objectsOfAlt :: Alt a A.Name -> Map A.Name (Type A.Name)
objectsOfAlt aa
 = case aa of
        AAlt p x        -> Map.union (objectsOfPat p) (objectsOfExp x)


-- Pat --------------------------------------------------------------------------------------------
objectsOfPat :: Pat A.Name  -> Map A.Name (Type A.Name)

objectsOfPat pp
 = case pp of
        PDefault        -> Map.empty
        PData _ bs      -> Map.unions (map objectsOfBind bs)


-- Bind -------------------------------------------------------------------------------------------
objectsOfBind
        :: Bind A.Name
        -> Map  A.Name (Type A.Name)

objectsOfBind bb
 = case bb of
        BNone _ -> Map.empty
        BAnon _ -> Map.empty

        BName n t
         | isHeapObject t
         -> Map.singleton n t

         | otherwise
         -> Map.empty


-- Utils ------------------------------------------------------------------------------------------
-- | Checks if we have a `Ptr# r Obj`.
isHeapObject :: Type A.Name -> Bool
isHeapObject t
 = case A.takeTPtr t of
        Nothing      -> False
        Just (_, tp) -> tp == A.tObj


