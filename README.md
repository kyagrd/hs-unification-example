# hs-unification-example
Implementation of the rule-based unification algorithm U using unbound.

Here is how to run the unification `f(x,f(y)) =?= f(z,x)`.
```
kyagrd@kyagrd:~/github/kyagrd/hs-unification-example$ stack ghci
...
Ok, modules loaded: Main, Syntax.
Loaded GHCi configuration from /tmp/ghci10172/ghci-script
*Main Syntax> u ([ D"f"[V x,D"f"[V y]] `Eq` D"f"[V z,V x] ], Data.Map.Strict.empty)
fromList [(x,D "f" [V y]),(z,D "f" [V y])]
```
That is, `{ x |-> f(y), z |-> f(y) }` as expected.

Occurs check indirectly by via other variables:
```
*Main Syntax> u ([V x `Eq` D"f"[V y], V y `Eq` D"f"[V z], V z `Eq` D"f"[V x]], Data.Map.Strict.empty)
*** Exception: user error (z occurs in D "f" [D "f" [D "f" [V z]]])
```
