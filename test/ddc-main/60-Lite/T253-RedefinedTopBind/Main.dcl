
module Main
exports {
        main      :: [r : %]. Nat# -> Ptr# r String# -> Int#;
}
with letrec

main [r : %] (x : Ptr# r String#) : Int#
 = 5i#

main [r : %] (x : Ptr# r String#) : Int#
 = 2i#
