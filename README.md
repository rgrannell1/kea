
Kea 0.67.0 [![Build Status](https://travis-ci.org/rgrannell1/kea.png)](https://travis-ci.org/rgrannell1/kea)
-----------------------------------
[![Gitter](https://badges.gitter.im/Join Chat.svg)](https://gitter.im/rgrannell1/kea?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

> *'By relieving the brain of all unnecessary work, a good notation sets it free to concentrate on more advanced problems, and, in effect, increases the mental power of the race.' -- Alfred N. Whitehead*

Kea is a functional programming library built to do three things:

* maximise developer happiness
* be quick to write, debug, and run
* make writing large programs easy

In practice, Kea is a collection of higher-order functions and standard collection algorithms that abstracts over the difference between atomic vectors, lists and pairlists. In this way Kea resembles the STL for C++ or Underscore for JavaScript. 

### Installation

The best way to get Kea is through github. You must install from the releases branch; Kea is in rapid development and breaking changes are frequently made.

```splus
if (!require(devtools)) {
    install.packages("devtools"); library(devtools)
}

devtools::install_github("rgrannell1/kea", ref = 'releases')
```

Go [here](https://github.com/rgrannell1/kea-snippets) for accompanying Sublime Text 3 snippets.

### Syntax

#### - Partial Application

Kea functions are partially applicable; they don't require all their arguments be supplied at once.

```r
xIsMatch('[0-9]+')
# -- base R syntax
function (str) xIsMatch('[0-9]+', str)

# -- xIsMatch is given a regular expression, then mapped over some strings.

xMap(xIsMatch('[0-9]+'), c('123', 'abc'))
# list(True, False)
```

#### - Methods

Every function has a corresponding method. To call a method you first pass data
to the kea object constructor (`x_`) and then call methods using the `$` operator.

```r
x_(1:10) $ xMap(sqrt) $ x_Reduce('+')
# 22.46828
```

#### - Arrow Functions

Arrow functions are a shorthand notation for creating functions:

```r
x := x

# base library.
function (x) x

(a : b) := {
	(a + b) / b
}

# base library
function (a, b) {
	(a + b) / b
}
```

#### - Wildcard Operators

Wildcard operators fix the right operand of an operator, leaving a function that takes the left operand.

```r
xMap(x. + 1, 0:3)
# 1:4
```

#### - Variadic Functions

Most collection functions in Kea have forms that take a collection of items, or an indeterminate number of items through the ellipses parametre ( ... ). The latter form is denoted with an underscore suffix and is more succinct, but less flexible than its alternative.

```r
xMap (x := x, list(1, 2, 3))
xMap_(x := x, 1, 2, 3)
```

#### - Function Composition

Function composition joins multiple functions end-to-end into a new function, that applies both transformations to an input in order.

```r
birds <- list(list('kea', 'New Zealand'), list('grouse', 'Ireland'))
xMap(xFirstOf %then% xCharsOf, birds)

list(3, 6)
```

Special operators are also available for combining predicate functions.

```r
xSelect_(is.integer %or% is.double, 'a', 1, 2L, '3', 4)
list(1, 2, 4)
```

## Licensing

**Kea** is released under the terms of the GNU General Public License version 3.

<img src="https://raw.githubusercontent.com/rgrannell1/kea/develop/gpl3.png" height = "120"> </img>

## Versioning

All versions post-release will be compliant with the Semantic Versioning 2.0.0 standard.

http://semver.org/

## Authors

Maintained and developed by Ryan Grannell.
