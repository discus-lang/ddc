
-- This module contains an unterminated block comment,
-- which the offside rule code needs to give a sensible error for.

module Main 
export {
        main    :: [r : Region]. Nat# -> Ptr# r String# -> Int#;
}
with letrec

main    [r : Region] 
        (argc : Nat#)   
        (argv : Ptr# r String#)
        : Int#
 = do   0i#


{-

