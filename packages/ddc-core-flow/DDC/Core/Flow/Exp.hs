
module DDC.Core.Flow.Exp
        ( module DDC.Core.Exp.Simple 
        , KindEnvF, TypeEnvF
        , TypeF
        , ModuleF
        , ExpF
        , CastF
        , LetsF
        , AltF
        , PatF
        , WitnessF
        , BoundF
        , BindF)
where
import DDC.Core.Module
import DDC.Core.Flow.Prim
import DDC.Core.Exp.Simple
import DDC.Type.Env             (Env)

type KindEnvF   = Env Name
type TypeEnvF   = Env Name

type TypeF      = Type Name

type ModuleF    = Module  () Name
type ExpF       = Exp     () Name
type CastF      = Cast    () Name
type LetsF      = Lets    () Name
type AltF       = Alt     () Name
type PatF       = Pat Name
type WitnessF   = Witness () Name

type BoundF     = Bound Name
type BindF      = Bind Name
