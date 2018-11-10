# catalan-magma
Combinatorial structures counted by Catalan numbers.

This Haskell code implements some of the ideas of the following paper [[1]](https://arxiv.org/abs/1808.09078):

```
@article{brak2018universal,
  title={A Universal Bijection for Catalan Structures},
  author={Brak, Richard},
  journal={arXiv preprint arXiv:1808.09078},
  year={2018}
}
```

The following assumes that the terminology of the paper [1] is known.

## Sample session

We can specify elements of the free magma by using the generator `Gen` and the product `:*:`:

```
*Main> let x = (Gen :*: Gen) :*: (Gen :*: Gen)
*Main> let y = Gen :*: Gen :*: Gen :*: Gen
```

The `DyckWord` magma consists of all strings of matching braces `{}`. `fromFree` maps from the free magma to a specific magma, carrying out the multiplications specified:

```
*Main> fromFree x :: DyckWord
DyckWord "{}{{}}"
*Main> fromFree y :: DyckWord
DyckWord "{}{}{}"
```

We can get corresponding elements of other magmas in the same way:

```
*Main> fromFree x :: PermAvoiding321
PermAvoiding321 [1,3,2]
*Main> fromFree x :: TwoRowTableau
TwoRowTableau [1,3,4] [2,5,6]
*Main> fromFree x :: FriezePattern
FriezePattern [2,1,3,1,2]
```

`toFree` is the isomorphisom mapping any magma to the free magma:

```
*Main> toFree (DyckWord "{{}}{}")
(Gen :*: (Gen :*: Gen)) :*: Gen
```

Combining these two isomorphisms we get an isomorphism from any magma to any other magma called `toOther`:

```
*Main> let f = toOther (DyckWord "{{}}{}") :: FriezePattern
*Main> f
FriezePattern [2,2,1,3,1]
*Main> toOther f :: DyckWord
DyckWord "{{}}{}"
```

`enumerate` returns a list of all elements of a magma with a given norm. It is polymorphic and works for any `CatalanMagma`:

```
*Main> enumerate 4 :: [FreeMagma]
[((Gen :*: Gen) :*: Gen) :*: Gen,(Gen :*: (Gen :*: Gen)) :*: Gen,(Gen :*: Gen) :*: (Gen :*: Gen),Gen :*: ((Gen :*: Gen) :*: Gen),Gen :*: (Gen :*: (Gen :*: Gen))]
*Main> enumerate 4 :: [DyckWord]
[DyckWord "{}{}{}",DyckWord "{{}}{}",DyckWord "{}{{}}",DyckWord "{{}{}}",DyckWord "{{{}}}"]
*Main> enumerate 4 :: [PermAvoiding321]
[PermAvoiding321 [1,2,3],PermAvoiding321 [2,1,3],PermAvoiding321 [1,3,2],PermAvoiding321 [3,1,2],PermAvoiding321 [1,3,2]]
*Main> enumerate 4 :: [TwoRowTableau]
[TwoRowTableau [1,3,5] [2,4,6],TwoRowTableau [1,2,5] [3,4,6],TwoRowTableau [1,3,4] [2,5,6],TwoRowTableau [1,2,4] [3,5,6],TwoRowTableau [1,2,3] [4,5,6]]```
*Main> enumerate 4 :: [FriezePattern]
[FriezePattern [3,1,2,2,1],FriezePattern [2,2,1,3,1],FriezePattern [2,1,3,1,2],FriezePattern [1,3,1,2,2],FriezePattern [1,2,2,1,3]]
```
