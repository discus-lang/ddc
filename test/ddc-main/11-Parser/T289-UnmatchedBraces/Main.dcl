
-- This module contains an extra misplaced close brace
-- which the offside rule code needs to give a sensible error for.
--
-- The point here is that the offside rule inserts a synthetic open 
-- brace after the 'do' but there is a manifest user-written one after it.

module Main 
exports {
        main    :: [r : %]. Nat# -> Ptr# r String# -> Int#;
}
with letrec

main    [r : %] 
        (argc : Nat#)   
        (argv : Ptr# r String#)
        : Int#
 = do   } 0i#

