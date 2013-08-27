Arrow v0.1
-----------------------------------

**Arrow** is a functional framework (inspired by Haskell, Clojure and JS), 
that adds dozens of higher-order functions and utility functions to the R language.

## 1 Installation

As of late August 2013 **Arrow** is only available on Github. To install the development version, copy the
following into an R console.

```javascript
install.packages('devtools')
require(devtools)

install_github("arrow", "rgrannell1", "develop")
require(devtools)

# check that arrow installed.
xVersion()
```
All **Arrow** functions are prefixed with the letter "x". This is to avoid naming conflicts and to 
help the user find the function they are looking for.

## 2 Features

### 2.1 Generic & Idiomatic

In general **Arrow** functions are as generic with respect to input type as possible,
though their output types are more rigidly defined.

```javascript
identical(
    xMap( function (x) x, 1:10),
    xMap( function (x) x, as.list(1:10)) )
identical(
    xMap( function (x) x, 1:10),
    xMap( function (x) x, as.pairlist(1:10)) )

# transitively xMap vector == xMap pairlist
```
Having a fixed output type makes arrow functions very easy to compose; Functions that normally
return integers will never return a NULL for some corner case.
Refreshingly, it also means that your code won't use ```if(is.null(x))``` like
full stops in an essay.

```javascript
xSubString('alonzo-church', c(1:3, 5, 7))
'alonoc'
```

R is unusual (in a good way) in that numbers and other values are always wrapped in 
vectors, so base functions operate on whole vectors as well as single values.
**Arrow** is vectorisation-friendly; where possible functions operate on vectors 
of values.

### 2.2 Functional

Arrow includes the standard map, select, fold, zip, flip, dropwhile, and position higher-order
functions (among others) as well as function composition and partial application.

```javascript

unlapply <- unlist %of% lapply
function (X, FUN, ...) 
{
    fn1(fn2(X, FUN, ...))
}

```

Functions that return functions - like ```xCompose()``` - preserve parameter names 
and produce human-readable code.

```javascript
isPrime <- function (n) {
    n == 2 || all(n %% 2:(n-1) != 0)
}
getPrimes <- xSelect(isPrime)
getPrimes(1:1000)

[1] 2  3  5  7 11 13 17 19 23 29 31 ...
```

FP allows a declarative style of programming; rather than using 
looping and pushing and pulling values into and out of containers, you focus more on 
the definition of the problem in terms of common patterns like filtering lists, 
or accumulating a value by recursing over a list.

Note that the user is not required to know the Lambda Calculus, or to understand monoids
in the category of endofunctors [1] to use this library; functions are only 
included in **Arrow** if they have a plausable use-case, and 
even then their mathematical underpinnings are masked [2].

### 2.3 Arrow Functions

**Arrow** is a functional library, with arrow functions; a shorthand syntax for unary functions. [4]

```javascript
# a polynomial equation
xMap(x := 2*x^2 * 3*x + 1, 1:1000)

xSelect(
    # grab all underscored vars in base.
    name := {
        grepl('_', name)
    },
    ls('package:base')
)

```

### 2.4 Cascading Style

In this style data is fed into the type constructor [1] ```x_```, and methods are called off that object. 
This small program gets every parametre used in the R base library.

```javascript
x_(ls("package:base"))$  
xMap(function (x) get(x))$
xSelect(is.function)$
xMap(xParametres)$
xReducel(union)$
x()
```

The final method - ```x()``` - takes the data out the object constructed by ```x_()``` 
for normal R functions to operate on.

### 2.5 Partial Application & Currying

Specialising general functions like select and fold is simple in **Arrow**.

```javascript
# be gone, na values!
xPartial(xReject, list(pred = is.na))

function (coll) 
{
    fn(.Primitive("is.na"), coll)
}
<environment: 0x5e8eb38>
```
The crème de la crème of **Arrow** is that it implements flexible
Haskell-style partial function application:

```javascript
isOdd <- function (x) {
    x %% 2 == 1
}
getOdd <- xSelect(isOdd)
getOdd(1:10)
```
This allows general functions like fold and select to be specialised, 
with no syntactic noise! 

### 2.6 Combinators

Combinators are powerful functions that combine functions in interesting ways. **Arrow** implements many 
combinators, giving them a formal name (eg. ```xPhi```), a descriptive name (eg. ```xBiCompose```) and
most importantly, an avian name (```xPhoenix```)[3].

```javascript
func_add <- xPartial(xBiCompose, list(fn1 = "+"))
# equivelant to the function xPlus()
func_add(
    function (x) 2*x + x,
    function (x) 3*x + x
)(1:100)
```

In fact combinators are so powerful that the ```xI``` (identity), ```xK```
(return a constant function), and 
```xS``` (a substitution combinator) that they in themselves can define a programming language!

```javascript
x_(1:100)$
xSelect( xOr(
    # two uncommon properties
    function (n) n^2 == 2^n,
    function (n) n*2 == n*n
) )

xMod( function (n) n^2, xK(6) )(1:4)
[1] 1 4 3 4
```

Of course, this is a less likely use of combinators than defining
your own control structures for functions. Arrow particularily emphasises 
arithmetic on functions, with several functions with short names added for that purpose.

## 3 Footnotes

[1] I won't use *that* word; every mention of *that* word cuts the usership of an FP library by half.

[2] This is a good thing: the worst example of overally mathematical code I saw while researching FP libraries was 
a function called a zygohistomorphic propremorpism. Useful concept I'm sure, but one with a horrible name.

[3] Raymond Smullyan's incredible *To Mock a Mockingbird* aliases combinators like K with a 
birdname (kestrel). These names are used fairly often, so I included them.

[4] Not to be confused with Arrows, the more general cousin of the Category-that-shall-not be named.

## 4 Licensing

**Arrow** is released under the terms of the GNU General Public License version 3. 

<img src="gpl3.png" height = "180"> </img>
