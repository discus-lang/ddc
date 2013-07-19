
module Main
exports {
        main      :: [r : Region]. Nat# -> Ptr# r String# -> Int#;
}
with letrec

main [r : Region] (x : Ptr# r String#) : Int#
 = 5i#

main [r : Region] (x : Ptr# r String#) : Int#
 = 2i#
