Arrow v0.1
-----------------------------------

**DISCLAIMER: Arrow is in heavy development and is highly liable to change.**

**Arrow** is a functional programming framework that adds partial application,
jQuery-like method chaining, function composition,
and over one-hundred higher-order- and utility-functions to the R language.
Arrow helps make R an elegant functional language with powerful operations on collections.

## 0 Goal

R is an excellent statistical platform, but it isn't a great general purpose programming language.
Libraries like ```plyr``` and ```stringr``` help, but they don't go far enough. I've 
always felt that the best aspects of the R language are its powerful functions, vectorisation and 
list data structure. Arrow is an attempt to build upon these aspects of R and to generalise
them into a pleasant, composable functional language.

## 1 Installation

As of late September 2013 **Arrow** is only available on Github. To install the development version, copy the
following into an R console.

```javascript
install.packages('devtools')
require(devtools)

install_github("arrow", "rgrannell1", "develop")
require(devtools)

# check that arrow installed.
xVersion()
```
**Arrow** functions are usually prefixed with the letter 'x'.

## 2 Examples

#### 2.1 Arrow Functions:

**Arrow** included a shorthand function constructor like that
 included in Scala or ECMAscript-6.

```javascript
x := x^2

function (x) x^2

(a : b) := {
    a + b
}
function (a, b) {
    a + b
}
```
#### 2.2 Function Composition:

**Arrow** includes powerful function-composition functions.
These functions produce human-readable code.

```javascript
unlapply <- unlist %of% lapply

function (X, FUN, ...)
{
    fn1(fn2(X, FUN, ...))
}
```
#### 2.3 Collection Manipulation

Most functions in **Arrow** are for manipulating collections of values.

```javascript
isPrime <- function (n) {
    n == 2 || all(n %% 2:(n-1) != 0)
}
getPrimes <- xSelect(isPrime)
getPrimes(1:1000)

[1] 2 3 5 7 11 13 17 19 23 29 31 ...
```
#### 2.4 Method Chaining:

**Arrow** includes jQuery-style method chaining for stepwise manipulation
of collections.

```javascript
x_( 1:100 )$
xSelect( x := x %% 2 == 0 )$
xReduce("+")$x()
```

Custom methods or anonymous functions can be added to the ```x_()``` object.

```javascript
x_()$xGraft('xMean', mean)

x_(1:100)$xMean$x()
x_(1:100)$xTap(mean)$x()
```

#### 2.5 Partial Application

General functions like ```xSelect``` and ```xFold``` can be
specialised by fixing some of their arguments permenantly.

```javascript
strip_na <- xPartial...(xReject, fn = is.na)
```
#### 2.6 Combinators

Combinators combine functions into other functions or control structures. These
can be used to define arithmetic, logical and set operations "on functions".

```javascript
my_mean <- xJuxtapose(mean, sd)
my_mean(1:10)

# add two functions together
f <- xPlusLift(
    n := n^2,
    n := n + n)
```

#### 2.7 Existential Quantifiers

Existential quantifiers test whether a predicate is true for any or all
elements in a collection, or the set product of multiple collections.

```javascript
xExists(
    (x: y) := {
        x^( floor(x) / y ) == x^y
    },
    1:100, 1:100
)
```

#### 2.8 Immutable Values

**Arrow** wraps native R code for locking variable names to a single value
permanently, making them easier to use.

```javascript
xVal(n, 10)
n <- 11

Error: cannot change value of locked binding for 'n'
```

## 3 Help

To get the documentation for a particular function from an R console simply prefix that function with a question mark.

```
?xSelect
```

## 4 Contributing

I'm (as of November 2013) working on **Arrow** alone. However, the library is very large and I feel that it
would benefit from community contribution, ranging from feedback to active development. 

#### 4.1 Library Development
* developing a stable function decorator that behaves like haskell's autocurrying.
* improving efficiency and composability of functions.
* improving exception handing within **Arrow**.

#### 4.2 Tools Development
* Improving the ```forall``` testing tool, and test case generators.
* Improving the ```time_profile``` benchmarking tool.
* Contributing to ```roxygen2``` and ```devtools```, which **Arrow** currently uses.

#### 4.3 Miscelleneous Tasks
* consistency and idiomaticy police; are there any corner cases that need tweaking, or functions that need renaming.
* improving tests, benchmarks and documentation (obviously!)
* suggestions for functions to implement.
* general feedback.
* filing bug reports.

For information about contributing, send me a message at @RyanGrannell on twitter.

#### 4.4 Design
* setting up a Github page for **Arrow**, possibly using ``staticdocs`` and bootstrap.
* designing a logo for **Arrow**.


## 5 Authors

Ryan Grannell.

## 6 Licensing

**Arrow** is released under the terms of the GNU General Public License version 3.

<img src="gpl3.png" height = "180"> </img>

