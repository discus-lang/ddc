-- | Type checker for the Disciple Core language.
--
--   The functions in this module do not check for language fragment compliance.
--   This needs to be done separately via "DDC.Core.Fragment".
--
module DDC.Core.Check
        ( -- * Configuration
          Config(..)
        , configOfProfile

          -- * Type checker trace
        , CheckTrace (..)

          -- * Checking Modules
        , checkModule

          -- * Checking Types
        , checkType,    checkTypeM
        , checkSpec
        , kindOfSpec
        , sortOfKind

          -- * Checking Expressions
        , Mode   (..)
        , Demand (..)
        , checkExp,     typeOfExp

          -- * Checking Witnesses
        , checkWitness, typeOfWitness
        , typeOfWiCon

          -- * Kinds of Constructors
        , takeSortOfKiCon
        , kindOfTwCon
        , kindOfTcCon

          -- * Annotations
        , AnTEC(..)

          -- * Error messages
        , Error         (..)
        , ErrorType     (..)
        , ErrorData     (..))
where
import DDC.Core.Check.Judge.Kind
import DDC.Core.Check.Judge.Kind.TyCon
import DDC.Core.Check.Judge.Module
import DDC.Core.Check.Judge.Witness
import DDC.Core.Check.Error
import DDC.Core.Check.Exp
import DDC.Core.Check.Base


-- | Check a type in the given universe with the given environment
--   Returns the updated type and its classifier (a kind or sort),
--   depeding on the universe of the type being checked.
checkType  :: (Ord n, Show n, Pretty n)
           => Config n -> Universe -> Type n
           -> Either (Error a n) (Type n, Type n)

checkType config uni tt
 = evalCheck (mempty, 0, 0)
 $ do   (t, k, _) <- checkTypeM config emptyContext uni tt Recon
        return (t, k)


-- | Check a spec in the given environment, returning an error or its kind.
checkSpec  :: (Ord n, Show n, Pretty n)
           => Config n -> Type n
           -> Either (Error a n) (Type n, Kind n)

checkSpec config tt
 = evalCheck (mempty, 0, 0)
 $ do   (t, k, _) <- checkTypeM config emptyContext UniverseSpec tt Recon
        return (t, k)


-- | Check a spec in an empty environment, returning an error or its kind.
kindOfSpec
        :: (Ord n, Show n, Pretty n)
        => Config n -> Type n
        -> Either (Error a n) (Kind n)

kindOfSpec config tt
 = evalCheck (mempty, 0, 0)
 $ do   (_, k, _) <- checkTypeM config emptyContext UniverseSpec tt Recon
        return k


-- | Check a kind in an empty environment, returning an error or its sort.
sortOfKind
        :: (Ord n, Show n, Pretty n)
        => Config n -> Kind n
        -> Either (Error a n) (Sort n)

sortOfKind config tt
 = evalCheck (mempty, 0, 0)
 $ do   (_, s, _) <- checkTypeM config emptyContext UniverseKind tt Recon
        return s

