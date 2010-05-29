{-# OPTIONS -fwarn-incomplete-patterns -fwarn-unused-matches -fwarn-name-shadowing #-}

-- | Instances for simple types. 
--	These are always printed the same way, independent of the mode.
module DDC.Util.Pretty.Simple where

import DDC.Util.Pretty.Base
import DDC.Util.Pretty.Combinators

import Data.Map			(Map)
import Data.Set			(Set)
import Data.Sequence		(Seq)
import Data.Foldable		(foldr)
import qualified Data.Map	as Map
import qualified Data.Set	as Set
import Prelude			hiding (foldr)



-- Base types -----------------------------------------------------------------
instance Pretty () m where
 ppr ()	= ppr "()"

instance Pretty Int m where
 ppr x	= plain $ PString $ show x

instance Pretty Char m where
 ppr x	= plain $ PChar x

instance Pretty Bool m where
 ppr x	= plain $ PString $ show x

instance Pretty Integer m where
 ppr x	= plain $ PString $ show x

instance Pretty Float m where
 ppr x 	= plain $ PString $ show x

instance Pretty Double m where
 ppr x 	= plain $ PString $ show x


-- Maybe ----------------------------------------------------------------------
instance Pretty a m => Pretty (Maybe a) m where
 ppr (Just x)	= "Just " % x
 ppr Nothing	= ppr "Nothing"


-- Either ---------------------------------------------------------------------
instance (Pretty a m, Pretty b m) => Pretty (Either a b) m where
 ppr (Left x)	= "Left " % x
 ppr (Right x)	= "Right " % x


-- List -----------------------------------------------------------------------
instance Pretty a m => Pretty [a] m where
 ppr xs
 	= PrettyM (\m -> PList 	$ map (\x -> case ppr x of
						PrettyM fx	-> fx m)
				  xs)


-- Sequence -------------------------------------------------------------------
instance Pretty a m => Pretty (Seq a) m where
 ppr xs	= ppr 
	$ foldr (:) [] xs


-- Tuples ---------------------------------------------------------------------
instance (Pretty a m, Pretty b m) 
	=> Pretty (a, b) m 
 where	ppr (a, b)	
 	 = "(" % a % ", " % b % ")"

instance (Pretty a m, Pretty b m, Pretty c m) 
	=> Pretty (a, b, c) m 
 where	ppr (a, b, c)
 	 = "(" % a % ", " % b % ", " % c % ")"

instance (Pretty a m, Pretty b m, Pretty c m, Pretty d m) 
	=> Pretty (a, b, c, d) m 
 where	ppr (a, b, c, d) 
 	 = "(" % a % ", " % b % ", " % c % ", " % d % ")"


-- Maps -----------------------------------------------------------------------
instance (Pretty a m, Pretty b m) 
	 => Pretty (Map a b) m 
 where	ppr mm 
 	 	= braces
		$ punc ", " 
		$ map (\(x, y) -> x % " := " % y)
		$ Map.toList mm
 

-- Sets -----------------------------------------------------------------------
instance (Pretty a m) 
	 => Pretty (Set a) m 
 where	ppr ss	
 		= braces
		$ punc ", "
		$ Set.toList ss
