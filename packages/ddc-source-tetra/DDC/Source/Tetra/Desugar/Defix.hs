
module DDC.Source.Tetra.Desugar.Defix
        ( Error         (..)
        , InfixTable    (..)
        , InfixDef      (..)
        , InfixAssoc    (..)
        , defixModule)
where
import DDC.Source.Tetra.Module
import DDC.Source.Tetra.Exp


-- | Things that can go wrong when defixing code.
data Error
        = ErrorBlerk
        deriving Show


-- | Table of infix operator definitions.
data InfixTable a n
        = InfixTable [InfixDef a n]


-- | Infix operator definition.
data InfixDef a n
        = InfixDef
        { -- | String of the operator.
          infixDefSymbol        :: String
        
          -- | Expression to rewrite the operator to.
        , infixDefExp           :: Exp a n

          -- | Associativity of infix operator.
        , infixDefAssoc         :: InfixAssoc
        
          -- | Precedence of infix operator.
        , infixDefPrec          :: Int }


-- | Infix associativity.
data InfixAssoc
        -- | Left associative.
        --      x * y * z => * (* x y) z
        = InfixLeft

        -- | Right associative.
        --      x * y * z => * x (* y z)
        | InfixRight

        -- | Non associative.
        --      x * y * z => error
        | InfixNone
        deriving (Show, Eq)


defixModule    
        :: InfixTable a n
        => Module a n 
        -> Either Error (Module a n)

defixModule _table mm 
        = Right mm

