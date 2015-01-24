This document outlines the implementation of lenses in Kea. (DRAFT)

### 0 Motivation

Lenses are getter-setter pairs satisfying certain common-sense laws. They are useful for deep updates in functionally-pure languages.

#### 0.1 Motivating Examples

```r
a <- list( b = list(c = list(d = 0)) )
a $ b $ c $ d <- a $ b $ c $ d + 1
```
Nested edits are unpleasant, but with use of function composition can be done effectively.
```r
a <- list( b = list(c = list(d = 0)) )
x_(a) $ xInvoke(
    xatKey(xI, 'b') %then% xatKey(xI, 'c') %then% .xatKey('d', x. + 1))
```


```r
x_(10) $ xIndicesTo()  $ xMap(xIndicesTo %then% xs := {
    xPrepend(xFirstOf(xs) * 2, xRestOf(xs))
})
```
The lens solution is cleaner; it doesn't require the destructure-map-restructure step.
```r
x_(10) $ xIndicesTo()  $ xMap(xIndicesTo %then% xfirstOf(x. * 2))
```

### 1 Laws
---
* **recoverability**: Setting a thing then getting it returns that value, unchanged.
* **reset identity**: getting a value and setting with it is the same as doing nothing.
* **update idempotence**: setting twice with a value is the same as setting once.

#### 1.1 Recoverability

Here the first element is set to 10, then retrieved. It returns 10, as expected.

```r
.xfirstOf(xI, xfirstOf(xK(10), 1:3))
# 10
````

This law prevents predicate-based lensing. Selecting where a predicate is true and replacing those values doesn't work, as the replacement might not be true for that predicate.

This law also restricts multiple-replacement to being one-to-one. For example, replacing the values at odd indices with a single value violates recoverability, but it is fine to replace them with a collection of equal length.

This law seems to be the most easily violated of the three, and should be tested for each lens.

```r
.xodds(xI, xodds(xK(1), 1:4))
# 1
```

#### 1.2 Reset Identity

The reset identity states that the subject extracted with a lens can be put back into the lens without any effect.

```r
coll <- 1:3
xfirstOf(x := .xfirstOf(xI, coll), coll)
#1:3
```

#### 1.3 Update Idempotence
```r
res <- xfirstOf(xK('a'), 1:3)
# 'a', 2, 3

xfirstOf(xK('a'), res)
# 'a', 2, 3
```

### 2 Syntax
---
Lens components will use a lowercase letter after their x-prefix to avoid masking normal functions with the same name.

#### 2.1 Getters

The getter component of the lens is denoted with a dot-prefix. They have the general type signature
`(any -> any) -> any -> any`. Functions that (when partially applied) produce lenses will also use this notation.

```r
# -- getters

.xfirstOf(x. + 1, 1:3)
# 2

.xfirstOf_(x. + 1, 1, 2, 3)
# 2
```
The function argument takes the subject of the lens as an argument, and returns a transformation of value. Getters are formulated this way to make them compose without chaining:

```
.xfirstOf(.firstOf, 1:10)
```

#### 2.2 Setters

The setter component lacks a dot-prefix. They have the type signature `(any -> any) -> any -> any` too. They apply a function to the subject of the lens, and replace the old subject in the lensed value.

```r
# -- setters

xfirstOf(x. + 1, 1:3)
# 2 2 3

xfirstOf_(x. + 1, 1, 2, 3)
# 2 2 3
```
### 3 Composition
---
The selling point of using lenses is that they are *composable* alternatives to getters and setters.

#### 3.1 Getter Composition

Under this scheme, getter composition isn't too painful.

```r
coll <- list(list(1), list(2))
.xfirstOf(.xfirstOf(xI), coll)
# 1
```
Here the inner call to `.xfirstOf` is partially applied with identity, to make it a true getter.

#### 3.2 Setter Composition

#### 4 Initial Lenses
---
A handful of lenses should be implemented in the initial release to validate the assumptions in this document. These should correspond to the use cases that R's base sub-setting and replacement operators achieve.

#### 4.1 `xidentity`

This lens is basically in Kea already, under the moniker of `xInvoke`. Vacuously satisfies the lens laws.

```r
.xidentity(identity, val)
# val
xidentity(identity, val)
# val
```
#### 4.2 `xat`

A lens factory. Constructs a lens for getting at setting a particular value in a collection, much like the operators `[[` and `[[ <-`.

Constructs *partial* lenses; if an attempt is made to set or get a position that doesn't exist an error is thrown. This ensures that the reset identity and recoverability hold over the domain of valid inputs; they would be violated if getting from `length + 1` was forbidden but setting was fine.

```r
x_(letters) $ .xat(xI, 26)
# z
```
#### 4.3 `xatKey`

Another lens factory. Implements the key-based component of  `[[` and `[[ <-`. Behaves like `xat` in most ways.

#### 4.4 `xfirstOf`, `xsecondOf`, `xthirdOf`, `xfourthOf`, 'xlastOf'

These partial lenses are specialisations of `xat`, and follow the same rules.

#### 4.5 `xtake`, `xdrop`

These total lenses treat their subject as a single 'virtual' element; they can update the head or tail of the collection in any way. They do not preserve the true collection length, but the result will always have a fixed block (part one) and a virtual head or tail (part two) that can be updated.

**Recoverability**

The contents of the head or tail of a collection can always be extracted as a list, without ambiguity.

**Reset Identity**

Extracting the subject with these lenses and resetting works as identity, as expected.

**Update Idempotency**

Repeatedly setting the head or tail with the same replacement should yield the same value, as expected.

#### 4.6 `xinitOf`, `xrestOf`

Specialisations of `xtake` and `xdrop`.

```r
xinitOf( x := 'a', 1:3 )
# 'a', 3
```

#### 4.7 Similar Data Structures

* `xslice`, `xkeysOf`,  `xvaluesOf` aren't conceptually one container; there needs to be a one-to-one correspondence between the current length and replacement length, which isn't enforced by the lenses above. These functions are stricter structures than lenses.

#### 5 Pros & Cons
---
**Pros:**
* Won't mask existing functions.
* Doesn't require a separate data structure; lens components are normal functions.
* Doesn't require reformulation of method-chaining; if lenses were represented directly `x_` would create an `id` monad containing a special function `call` lens, and `$` would be lens composition. This would also require a terminal call to evaluate the method chain, breaking things like `xDo`. This formulation is simple.

**Cons:**
* Often requires `xI` to be passed into getters.
* Composition requires more boilerplate than a direct implementation of the lens data structure; deep updates turn introduce the Pyramid of Doom into R.
* Kea avoids high (>= 3) arity functions where possible, and this formulation adds an additional function argument to each lens component.
* Lenses themselves are a very limited abstraction; they only allow basic key and index updates, not predicate-based updates or slicing.
* Combinatorial explosion of name hacks; names now encode lens vs function, chaining vs unchaining (methods only), getter vs setter, and variadic vs non-variadic.
* Added complexity of library; this formulation of lenses is easy, but adds yet another thing to learn to Kea.
* The requirement that getters take a function is frustrating, as getters will not always transform their subject data.
* dot-prefixes denote hidden variables. This means lens getters won't work with the few functions like `ls` that use this special convention.
