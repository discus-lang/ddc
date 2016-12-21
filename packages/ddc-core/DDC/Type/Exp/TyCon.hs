
module DDC.Type.Exp.TyCon where
import Data.Text (Text)

-- | Sort constructor.
data SoCon
        -- | Sort of witness kinds.
        = SoConProp                -- 'Prop'

        -- | Sort of computation kinds.
        | SoConComp                -- 'Comp'
        deriving (Eq, Ord, Show)


-- | Kind constructor.
data KiCon
        -- | Function kind constructor.
        --   This is only well formed when it is fully applied.
        = KiConFun              -- (->)

        -- Witness kinds ------------------------
        -- | Kind of witnesses.
        | KiConWitness          -- 'Witness :: Prop'

        -- Computation kinds ---------------------
        -- | Kind of data values.
        | KiConData             -- 'Data    :: Comp'

        -- | Kind of regions.
        | KiConRegion           -- 'Region  :: Comp'

        -- | Kind of effects.
        | KiConEffect           -- 'Effect  :: Comp'

        -- | Kind of closures.
        | KiConClosure          -- 'Closure :: Comp'
        deriving (Eq, Ord, Show)


-- | Witness type constructors.
data TwCon
        -- Witness implication.
        = TwConImpl             -- :: '(=>) :: Witness -> Data'

        -- | Purity of some effect.
        | TwConPure             -- :: Effect  -> Witness

        -- | Constancy of some region.
        | TwConConst            -- :: Region  -> Witness

        -- | Constancy of material regions in some type
        | TwConDeepConst        -- :: Data    -> Witness

        -- | Mutability of some region.
        | TwConMutable          -- :: Region  -> Witness

        -- | Mutability of material regions in some type.
        | TwConDeepMutable      -- :: Data    -> Witness

        -- | Distinctness of some n regions
        | TwConDistinct Int     -- :: Data    -> [Region] -> Witness
        
        -- | Non-interfering effects are disjoint. Used for rewrite rules.
        | TwConDisjoint         -- :: Effect  -> Effect   -> Witness
        deriving (Eq, Ord, Show)


-- | Other constructors at the spec level.
data TcCon
        -- Data type constructors ---------------
        -- | The unit data type constructor is baked in.
        = TcConUnit             -- 'Unit :: Data'

        -- | Pure function, with explicitly provided argument.
        | TcConFunExplicit      -- '(->)' :: Data -> Data -> Data

        -- | Pure function, with implicitly provided argument.
        | TcConFunImplicit      -- '(~>)' :: Data -> Data -> Data

        -- | A suspended computation.
        | TcConSusp             -- 'S     :: Effect -> Data -> Data'

        -- | A record type constructor,
        --   with the given field names.
        | TcConRecord [Text]    -- '{n1 .. nn} :: Data -> ... Data -> Data

        -- Effect type constructors -------------
        -- | Read of some region.
        | TcConRead             -- Read      :: 'Region -> Effect'

        -- | Read the head region in a data type.
        | TcConHeadRead         -- HeadRead  :: 'Data   -> Effect'

        -- | Read of all material regions in a data type.
        | TcConDeepRead         -- DeepRead  :: 'Data   -> Effect'
        
        -- | Write of some region.
        | TcConWrite            -- Write     :: 'Region -> Effect'

        -- | Write to all material regions in some data type.
        | TcConDeepWrite        -- DeepWrite :: 'Data   -> Effect'
        
        -- | Allocation into some region.
        | TcConAlloc            -- Alloc     :: 'Region -> Effect'

        -- | Allocation into all material regions in some data type.
        | TcConDeepAlloc        -- DeepAlloc :: 'Data   -> Effect'
        deriving (Eq, Ord, Show)

