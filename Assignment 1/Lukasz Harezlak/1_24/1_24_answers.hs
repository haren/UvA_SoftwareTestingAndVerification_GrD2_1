[1 of 1] Compiling Main             ( 1_24.hs, interpreted )

1_24.hs:2:9:
    Couldn't match expected type `Integer'
                with actual type `Integer -> Integer'
    In the return type of a call of `ldpf'
    Probable cause: `ldpf' is applied to too few arguments
    In the expression: ldpf primes1
    In an equation for `ldp': ldp n = ldpf primes1
Failed, modules loaded: none.

There's a missing parameter for ldfp (expects two, now we're providing only one).

Not sure if got the question right.