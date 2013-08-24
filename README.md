Arrow v0.1
-----------------------------------

**Arrow** is a functional framework (inspired by Haskell, Clojure and JS), 
that adds dozens of higher-order functions and utility functions to the R language.

## Installation

As of late August 2013 **Arrow** is only available on Github. To install the development version, copy the
following to an R console.

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

## Features

### Generic & Idiomatic

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
Having a fixed output type makes arrow functions very easy to compose.
Refreshingly, it also means that your code won't use ```if(is.na(x))``` like
full stops in an essay.

```javascript
xSubString('alonzo-church', c(1:3, 5, 7))
'alonoc'
```

R is unusual (in a good way) in that numbers and other values are always wrapped in 
vectors, so base functions operate on whole vectors as well as single values.
**Arrow** is vectorisation-friendly; where possible functions operate on vectors 
of values.

### Functional

Arrow includes the standard map, select, fold, zip, flip, dropwhile, and position higher-order
functions (among others) as well as function composition and partial application.

```javascript

unlapply <- unlist %of% lapply
function (X, FUN, ...) 
{
    fn_1(fn_2(X, FUN, ...))
}

xFixDefaults(Reduce)

function (f, x, init) 
{
    fn(f, x, init, right = FALSE, accumulate = FALSE)
}
```

Functions that return functions - like ```xCompose()``` - preserve parameter names 
and produce human-readable code.

```javascript
enumPrimesTo <- function (to) {
    
    isPrime <- function (n) {
        n == 2 || all(n %% 2:(n-1) != 0)
    }
    
    xSelect(isPrime, 1:to)
}
enumPrimesTo(100)
```

FP allows a declarative style of programming; rather than using 
looping and pushing and pulling values into and out of containers, you focus more on 
the definition of the problem in terms of common patterns like filtering lists, 
or accumulating a value by recursing over a list.

Note that the user is not required to know the Lambda Calculus, or to understand monoids
in the category of endofunctors [1] to use this library.

### Cascading Style

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

### Partial Application & Currying







## Footnotes

[1] I won't use *that* word; every mention of *that* word cuts the usership of an FP library by half.

## Licensing

**Arrow** is released under the terms of the GNU General Public License version 3. 

<img src="gpl3.png" height = "180"> </img>
