# Machine fusion for DDC

## Machine fragment

### Primitives

#### Kinds and types
```
Static    : Kind

Stream    : Data -> Static

Source    : Data -> Static
Sink      : Data -> Static

Process   : Static
```

#### Conversion between Streams and Source/Sink
```
stream_i_o# :
    : [in_0..in_i : Data]
    . [out_0..out_o : Data]
    . (Source in_0 .. -> Source in_i -> Sink out_0 .. -> Sink out_o -> Process)
    -> Stream in_0 .. -> Stream in_i
    -> Stream out_0 .. * Stream out_o

process_i_o# :
    : [in_0..in_i : Data]
    . [out_0..out_o : Data]
    . (Stream in_0 .. -> Stream in_i -> Stream out_0 .. * Stream out_o)
    -> Source in_0 .. -> Source in_i
    -> Sink out_0 .. -> Sink out_o
    -> Process
```

#### Constructing processes
```
pull# : [i : Data]. Source i -> (i -> Process) -> Process
push# : [o : Data]. Sink o -> o -> Process -> Process
drop# : [i : Data]. Source i -> Process
case# : Bool -> Process -> Process -> Process
```

### Example combinators
```
map : (a -> b) -> Stream a -> Stream b
map f = stream_1_1# (\(in : Source a). \(out : Sink b).
 let p1   = pull# in p2
 let p2 v = push# out (f v) p3
 let p3   = drop# in p1
 p1)
```

### Fusing a process nest
```
mapmap : Stream Int -> Stream Int
mapmap xs = map (+1) (map (*2) xs)
```

```
process_1_1# mapmap : Source Int -> Sink Int -> Process

==>

process_1_1# (\xs : Stream Int.  map (+1) (map (*2) xs))

==>

process_1_1# (\xs : Stream Int.
    let m2 = stream_1_1# (\i o.
        let p1   = pull# i p2
        let p2 v = push# o (v * 2) p3
        let p3   = drop# in p1
        p1) xs
    let m1 = stream_1_1# (\i o.
        let q1   = pull# i q2
        let q2 v = push# o (v + 1) q3
        let q3   = drop# in q1
        q1) m2
    m1)

==>

process_1_1# (\xs : Stream Int.
    let m = stream_1_2# (\i1 o1 o2.
        let p1_q1_{}_{}       = pull# i1 p2_q1_{}_{}
        let p2_q1_{}_{}  v    = p2_q1_{p}_{} (v * 2)
        let p2_q1_{p}_{} v'o1 = push# o1 v'o1 p3_q1_{}_{a}
        let p3_q1_{}_{a} v'o1 = drop# i1 (p1_q1_{}_{a} v'o1)
        let p1_q1_{}_{a} v'o1 = p1_q2_{}_{a} v'o1
        let p1_q2_{}_{a} v    = push# o2 (v + 1) p1_q3_{}_{a}
        let p1_q3_{}_{a}      = p1_q1_{}_{}
        p1_q1_{}_{}) xs
    m)

==>

        \i1 o2.
        let p1_q1_{}_{}       = pull# i1 p2_q1_{}_{}
        let p2_q1_{}_{}  v    = p2_q1_{p}_{} (v * 2)
        let p2_q1_{p}_{} v'o1 = p3_q1_{}_{a}
        let p3_q1_{}_{a} v'o1 = drop# i1 (p1_q1_{}_{a} v'o1)
        let p1_q1_{}_{a} v'o1 = p1_q2_{}_{a} v'o1
        let p1_q2_{}_{a} v    = push# o2 (v + 1) p1_q3_{}_{a}
        let p1_q3_{}_{a}      = p1_q1_{}_{}
        p1_q1_{}_{}
```

