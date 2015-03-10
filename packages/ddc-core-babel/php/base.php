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

    function pap($f, $args) {
        return function() use ($f, $args) {
            $args2 = func_get_args();
            $args3 = array_merge($args, $args2);
            return DDC::call($f, $args3);
        };
    }
}

?>
