# hs-unification-example
Implementation of the rule-based unification algorithm U using unbound.

Here is how to run the unification `f(x,f(y)) =?= f(z,x)`.
```
kyagrd@kyagrd:~/github/kyagrd/hs-unification-example$ stack ghci
...
Ok, modules loaded: Main, Syntax.
Loaded GHCi configuration from /tmp/ghci11370/ghci-script
*Main Syntax Unif1> :l Main
*Main> u ([ D"f"[V x,D"f"[V y]] `Eq` D"f"[V z,V x] ], emptyMap)
fromList [(x,V z),(z,D "f" [V y])]
```
That is, `{ x |-> z, z |-> f(y) }` as expected.

Running the above example step-by-step:
```
*Main> ustep ([ D"f"[V x,D"f"[V y]] `Eq` D"f"[V z,V x] ], emptyMap)
([Eq (V x) (V z),Eq (D "f" [V y]) (V x)],fromList [])
*Main> ustep it
([Eq (D "f" [V y]) (V z)],fromList [(x,V z)])
*Main> ustep it
([Eq (V z) (D "f" [V y])],fromList [(x,V z)])
*Main> ustep it
([],fromList [(x,V z),(z,D "f" [V y])])
```


Occurs check indirectly by via multiple variables is working, for example, `x=?=f(y), y=?=f(z), z=?=f(x)`:
```
*Main> u ([V x `Eq` D"f"[V y], V y `Eq` D"f"[V z], V z `Eq` D"f"[V x]], emptyMap)
*** Exception: user error (z occurs in D "f" [D "f" [D "f" [V z]]])
```
Running the above failing example step-by-step:
```
*Main> ustep ([V x `Eq` D"f"[V y], V y `Eq` D"f"[V z], V z `Eq` D"f"[V x]], emptyMap)
([Eq (V y) (D "f" [V z]),Eq (V z) (D "f" [D "f" [V y]])],fromList [(x,D "f" [V y])])
*Main> ustep it
([Eq (V z) (D "f" [D "f" [D "f" [V z]]])],fromList [(x,D "f" [V y]),(y,D "f" [V z])])
*Main> ustep it
*** Exception: user error (z occurs in D "f" [D "f" [D "f" [V z]]])
```
