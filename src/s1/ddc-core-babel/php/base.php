<?php

class DDC {
    function curry($f, $num) {
        return function() use ($f, $num) {
            $args = func_get_args();
            if (count($args) < $num) {
                return DDC::curry(DDC::pap($f, $args), $num - count($args));
            } else {
                $as = array_slice($args, 0, $num);
                $bs = array_slice($args, $num);
                $ff = DDC::call($f, $as);
                if (count($bs)) {
                    return DDC::call($ff, $bs);
                } else {
                    return $ff;
                }
            }
        };
    }

    function call($f, $args) {
        return call_user_func_array($f, $args);
    }
    function apply() {
        $args = func_get_args();
        $f = array_shift($args);
        return call_user_func_array($f, $args);
    }

    function pap($f, $args) {
        return function() use ($f, $args) {
            $args2 = func_get_args();
            $args3 = array_merge($args, $args2);
            return DDC::call($f, $args3);
        };
    }



    function PrimArithNeg($x) { return -$x; }
    const PrimArithNeg_arity = 1;

    function PrimArithAdd($x, $y) { return $x + $y; }
    const PrimArithAdd_arity = 2;

    function PrimArithSub($x, $y) { return $x - $y; }
    const PrimArithSub_arity = 2;

    function PrimArithMul($x, $y) { return $x * $y; }
    const PrimArithMul_arity = 2;

    function PrimArithDiv($x, $y) { return $x / $y; }
    const PrimArithDiv_arity = 2;

    function PrimArithMod($x, $y) { return $x % $y; }
    const PrimArithMod_arity = 2;

    function PrimArithRem($x, $y) { return $x % $y; }
    const PrimArithRem_arity = 2;
//        -- comparison

    function PrimArithEq($x, $y) { return $x == $y; }
    const PrimArithEq_arity = 2;

    function PrimArithNeq($x, $y) { return $x != $y; }
    const PrimArithNeq_arity = 2;

    function PrimArithGt($x, $y) { return $x > $y; }
    const PrimArithGt_arity = 2;

    static function PrimArithGe($x, $y) { return $x >= $y; }
    const PrimArithGe_arity = 2;

    function PrimArithLt($x, $y) { return $x < $y; }
    const PrimArithLt_arity = 2;

    function PrimArithLe($x, $y) { return $x <= $y; }
    const PrimArithLe_arity = 2;

//        -- boolean

    function PrimArithAnd($x, $y) { return $x && $y; }
    const PrimArithAnd_arity = 2;

    function PrimArithOr($x, $y) { return $x || $y; }
    const PrimArithOr_arity = 2;
//
//        -- bitwise
//        | PrimArithShl  -- ^ Shift Left
//        | PrimArithShr  -- ^ Shift Right
//        | PrimArithBAnd -- ^ Bit-wise And
//        | PrimArithBOr  -- ^ Bit-wise Or
//        | PrimArithBXOr -- ^ Bit-wise eXclusive Or
}

?>
