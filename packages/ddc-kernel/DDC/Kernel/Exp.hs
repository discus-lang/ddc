{-# LANGUAGE ScopedTypeVariables #-}
module DDC.Kernel.Exp
        ( Exp (..)
        , mapAnnot
        , mapBindsBounds)
where
import Data.List
import Prelude hiding (abs)


-------------------------------------------------------------------------------
-- | Disciple Kernel language.
--
--   The least possible number of constructors for a usable 
--   functional core language, or triple your money back.
--
--   Parameterised by the types for.
--      @a@-nnotations, 
--      @b@-inders, 
--      bo-@u@-nds,  and
--      @k@onstants.
--
data Exp a b u k

        -- | Abstraction, which combines both lamdba and letrec functionality.
        --   Given the abstraction: 
        --           @([b_1 .. b_s b_t .. b_n] {x_t .. x_n}. x_0)@
        --
        --   The binders @b_t@ .. @b_n@ recursively let-bind bind the expressions 
        --   @x_t .. x_n@, and are also in scope in @x_0@. 
        --  
        --   The binders @b_1 .. b_s@ are parameters to the function and are
        --   given values via application. They are in scope in the bindings
        --   as well as the body.
        --
        = XAbs a [b] [Exp a b u k] (Exp a b u k)

        -- | Apply an expression to some arguments.
        | XApp a (Exp a b u k) [u]

        -- | Literals.
        | XLit a  k
        deriving (Eq)


instance (Show b, Show u, Show k) => Show (Exp a b u k) where
        show (XLit _ k) = show k

        show (XAbs _ bs xsBinds xBody)
         =  "["  ++ intercalate " " (map show bs)      ++ "] "
         ++ "{"  ++ intercalate "; " (map show xsBinds) ++ "}"
         ++ ". "  ++ show xBody

        show (XApp _ xFun usArg)
         =  (case xFun of
                XAbs{} -> "(" ++ show xFun ++ ")"
                _      -> show xFun)
         ++ " <"  ++ intercalate " " (map show usArg)   ++ ">"


-- | Transform the annotations in an expression.
mapAnnot 
        :: (a1 -> a2)
        -> Exp a1 b u k -> Exp a2 b u k

mapAnnot f xx
 = case xx of
        XLit a k
         -> XLit (f a) k

        XAbs a b xs x
         -> XAbs (f a) b (map (mapAnnot f) xs) (mapAnnot f x)

        XApp a xF us
         -> XApp (f a) (mapAnnot f xF) us


-- | Transform the binds and bounds in an expression.
mapBindsBounds   
        :: (b1 -> b2)    -> (u1 -> u2) 
        -> Exp a b1 u1 k -> Exp a b2 u2 k

mapBindsBounds fb fu xx
 = case xx of
        XLit a k
         -> XLit a k

        XAbs a b xs x   
         -> XAbs a 
                (map fb b) 
                (map (mapBindsBounds fb fu) xs) 
                (mapBindsBounds fb fu x)

        XApp a xF us
         -> XApp a 
                (mapBindsBounds fb fu xF) 
                (map fu us)


