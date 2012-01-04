-- Imports

-- Pragmas

-- Infix

-- Data
data IntFun %r1 %r2 %r3 %r4 !e1 $c1
        = FInt {
                Base.Int32 %r1;
        }
        
        | FFun {
                Base.Int32 %r2 -(!e1 $c1)> Base.Int32 %r3;
        };


-- Effects

-- Regions

-- Classes

-- Class dictionaries

-- Class instances

-- Foreign imports

-- Binds
foreign import copyIntFun
        :: forall %r0 %r1 %r2 %r3 %r4 %r5 %r6 %r7 !e0 $c0
        .  IntFun %r0 %r1 %r2 %r3 !e0 $c0 -(!e1)> IntFun %r4 %r5 %r6 %r7 !Bot $Bot
        :- !e1        = Base.!Read %r0
        :$ Base.Data -> Base.Data;
        

