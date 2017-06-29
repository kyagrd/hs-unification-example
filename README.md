# hs-unification-example
Implementation of the rule-based unification algorithm U using unbound.

Here is how to run the unification `f(x,f(y)) =?= f(z,x)` using `Unif1` module.
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

There is an alternative implementation (`Unif2` module) of the algorithm U,
which expands the substitution mapping instead of expanding the rest of the equations.
The advantage of this alternative implementation is that "localized" to
the substitution mapping instead of repetitive cascading application of
substitutions to the rest of the equations. `Unif2` implementation can
easily be refactored to abstract the substitution mapping via the state monad.

# Implementing unification modulo subterm convergent rewriting

`Main` module contains an implementation of unification modulo subterm convergent rewriting,
which is a Haskell implementation of [SubVariant](http://www.lsv.fr/~ciobaca/subvariant/).
Here is a run of the same example form the SubVariant homepage:
```
*Main> ruleSet1
[bind [x,y](D "dec" [D "enc" [V 0@0,V 0@1],V 0@1],V 0@0)]

*Main> let t1 = D "dec" [V x,V y]; t2 = D "dec" [V z,V y]

*Main> mapM_ print $ nub $ fmap (\(a,b,c)->(a,c)) $ kleeneClosure (narrBy ruleSet1) (D"_"[t1,t2],subtermInit(D"_"[t1,t2]),emptyMap)
(D "_" [D "dec" [V x,V y],D "dec" [V z,V y]],fromList [])
(D "_" [V x4,D "dec" [V z,V y3]],fromList [(x,D "enc" [V x4,V y3]),(y,V y3)])
(D "_" [D "dec" [V x,V y3],V x4],fromList [(y,V y3),(z,D "enc" [V x4,V y3])])
(D "_" [V x4,V x4],fromList [(x,D "enc" [V x4,V y3]),(y,V y3),(z,D "enc" [V x4,V y3])])

*Main> mapM_ print $ nub $ unifiersModulo ruleSet1 (t1,t2)
fromList [(x,V z)]
fromList [(x,D "enc" [D "dec" [V z,V y3],V y3]),(y,V y3)]
fromList [(y,V y3),(z,D "enc" [D "dec" [V x,V y3],V y3])]
fromList [(x,D "enc" [V x4,V y3]),(y,V y3),(z,D "enc" [V x4,V y3])]

```
