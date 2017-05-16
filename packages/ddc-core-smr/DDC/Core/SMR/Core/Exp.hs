{-# LANGUAGE GADTs #-}
module DDC.Core.SMR.Core.Exp
        ( Name
        , Module        (..)
        , Decl          (..)
        , lookupDecl
        , Exp           (..)
        , Ref           (..)
        , takeXApps, makeXApps)
where
import Data.Text        (Text)


-- Name -----------------------------------------------------------------------
-- | Represent names as text.
type Name = Text


-- Module ---------------------------------------------------------------------
data Module s p where
        Module  :: [Decl s p] -> Module s p


deriving instance (Show s, Show p) => Show (Module s p)


-- Decl -----------------------------------------------------------------------
-- | Top-level declaration.
data Decl s p where
        DTerm   :: Name -> Exp s p -> Decl s p


deriving instance (Show s, Show p) => Show (Decl s p)


-- | Lookup the body of a declaration with the given name from a list.
lookupDecl
        (n      :: Name)
        (decls  :: [Decl s p])
 = case decls of
        []              -> Nothing

        DTerm n' x : ds
         | n == n'      -> Just x
         | otherwise    -> lookupDecl n ds


-- Exp ------------------------------------------------------------------------
-- | Generic expression language.
data Exp s p  where
        -- Functional core.
        XVar    :: Name    -> Int     -> Exp s p
        XAbs    :: Name    -> Exp s p -> Exp s p
        XApp    :: Exp s p -> Exp s p -> Exp s p

        -- Reference to an external thing.
        XRef    :: Ref s p -> Exp s p


-- | Reference to an external thing.
data Ref s p where
        -- Macro name.
        RMac    :: Name  -> Ref s p

        -- Set name.
        RSet    :: Name  -> Ref s p

        -- Uninterpreted symbol.
        RSym    :: s     -> Ref s p

        -- Primitive value.
        RPrm    :: p     -> Ref s p

deriving instance (Show s, Show p) => Show (Exp s p)
deriving instance (Show s, Show p) => Show (Ref s p)


-- | Make an application of a function to the given list of arguments.
makeXApps (xFun :: Exp s p) (xsArgs :: [Exp s p])
 = build (reverse xsArgs)
 where  
        build xs
         = case xs of
                []              -> xFun
                x1 : xsArgs'    -> XApp (build xsArgs') x1


-- | Take an application of a function to a list of arguments.
takeXApps (xx :: Exp s p)
 = case xx of
        XApp x1@(XApp _ _) a2
         -> case takeXApps x1 of
                Just (f1, as1)  -> Just (f1, (as1 ++ (a2 : [])))
                Nothing         -> Nothing

        XApp x1 a2
          -> Just (x1, (a2 : []))

        _ -> Nothing
