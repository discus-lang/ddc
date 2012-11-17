

module Main 
exports {
        main    :: [r : %]. Nat# -> Ptr# r String# -> Int#;
}
with letrec

main    [r : %] 
        (argc : Nat#)   
        (argv : Ptr# r String#)
        : Int#
 = do   0i#


{-

