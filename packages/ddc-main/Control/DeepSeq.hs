
-- | Deep strict evaluation support
--   Originally stolen from comp.lang.functional from Dean Herington
--   The `DeepSeq` class provides a method `deepSeq` that is similar to
--	`seq` except that it forces deep evaluation of its first argument
--	before returning its second argument.

--	Instances of `DeepSeq` are provided for Prelude types.  Other
--	instances must be supplied by users of this module.

module Control.DeepSeq
where
import qualified Data.Map	as Map
import Data.Map			(Map)


class  DeepSeq a  where
    deepSeq :: a -> b -> b
    deepSeq = seq

infixr 0 `deepSeq`, $!!

($!!) :: (DeepSeq a) => (a -> b) -> a -> b

f $!! x = x `deepSeq` f x

strict	:: DeepSeq a => a -> a
strict x
    = x `deepSeq` x

instance  DeepSeq ()
instance  DeepSeq Bool
instance  DeepSeq Char
instance  DeepSeq Ordering
instance  DeepSeq Int
instance  DeepSeq Integer
instance  DeepSeq Float
instance  DeepSeq Double
instance  DeepSeq (a -> b)
instance  DeepSeq (IO a)

instance  (DeepSeq a) => DeepSeq (Maybe a)  where
    deepSeq Nothing  y = y
    deepSeq (Just x) y = deepSeq x y

instance  (DeepSeq a, DeepSeq b) => DeepSeq (Either a b)  where
    deepSeq (Left  a) y = deepSeq a y
    deepSeq (Right b) y = deepSeq b y

instance  (DeepSeq a) => DeepSeq [a]  where
    deepSeq []     y = y
    deepSeq (x:xs) y = deepSeq x $! deepSeq xs y

instance  (DeepSeq a,DeepSeq b) => DeepSeq (a,b)  where
    deepSeq (a,b)           y = deepSeq a $! deepSeq b y

instance  (DeepSeq a,DeepSeq b,DeepSeq c) => DeepSeq (a,b,c)  where
    deepSeq (a,b,c)         y = deepSeq a $! deepSeq b $! deepSeq c y

instance  (DeepSeq a,DeepSeq b,DeepSeq c,DeepSeq d) => DeepSeq (a,b,c,d) where
    deepSeq (a,b,c,d)       y = deepSeq a $! deepSeq b $! deepSeq c $! deepSeq d y

instance  (DeepSeq a,DeepSeq b,DeepSeq c,DeepSeq d,DeepSeq e) => DeepSeq (a,b,c,d,e)  where
    deepSeq (a,b,c,d,e)     y = deepSeq a $! deepSeq b $! deepSeq c $! deepSeq d $! deepSeq e y

instance  (DeepSeq a,DeepSeq b,DeepSeq c,DeepSeq d,DeepSeq e,DeepSeq f) => DeepSeq (a,b,c,d,e,f)  where
    deepSeq (a,b,c,d,e,f)   y = deepSeq a $! deepSeq b $! deepSeq c $! deepSeq d $! deepSeq e $! deepSeq f y

instance  (DeepSeq a,DeepSeq b,DeepSeq c,DeepSeq d,DeepSeq e,DeepSeq f,DeepSeq g) => DeepSeq (a,b,c,d,e,f,g)  where
    deepSeq (a,b,c,d,e,f,g) y = deepSeq a $! deepSeq b $! deepSeq c $! deepSeq d $! deepSeq e $! deepSeq f $! deepSeq g y

instance (DeepSeq a, DeepSeq b) => DeepSeq (Map a b) where
    deepSeq mm y	= deepSeq (Map.toList mm) y

