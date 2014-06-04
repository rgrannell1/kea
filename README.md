
Arrow 0.14.0 [![Build Status](https://travis-ci.org/rgrannell1/arrow.png)](https://travis-ci.org/rgrannell1/arrow)
-----------------------------------

> *'By relieving the brain of all unnecessary work, a good notation sets it free to concentrate on more advanced problems, and, in effect, increases the mental power of the race.' -- Alfred N. Whitehead*

### Public Release: ~1 August 2014

Arrow makes R an effective language for functional programming.

### Installation

Arrow isn't (and probably won't be) on CRAN. This is to allow for frequent updates; it
is considered bad etiquette to update a CRAN package more than once a month.

```js
install.packages("devtools")
install_github("arrow", "rgrannell1", ref = "releases")
```

## What is Arrow?

Functional programming has become commonplace in languages like JavaScript
and Python, but R is conspicuously lacking such a library. Arrow is a functional
library for general-purpose programming in R. It adds all the common higher-order
functions (Map, Fold, Compose, ...) and functions taken from
set theory and combinatorics. Arrow also exploits R's flexibility to add
arrow functions, methods, wildcards & list-comprehensions to the language.

For library documentation and tutorials head to
[http://rgrannell1.github.io/arrow/](http://rgrannell1.github.io/arrow/).

## What Does Arrow Look Like?

First, a table of Arrow's (optional) new syntax.

```js
# function shorthands
x := 2 * x + 1                               # instead of function (x) 2 * x + 1
x. $ Species                                 # instead of function (x) x $ Species

# list comprehensions
xList[x, x <- 1:10, x %% 2 == 0]             # generates 2, 4, ..., 10

# function composition
(unlist %then% mean)(list(1, 2, 3))          # instead of ( function (x) mean(unlist(x)) )(list(1, 2, 3))
(is.integer %or% is.double %or% is.list)(1)  # instead of is.integer(1) || is.double(1) || is.list(1)

# methods!
x_(letters) $ xMap(toupper) $ x_FromChars()  # generates the string ABCD...Z
```

With that out the way, here is a simple use of Arrow to examine cocaine seizure data.

```js
# Data From Hadley Wickham's https://github.com/hadley/data-stride

asRow <- (...) := {
	list(state = ..1, potency = ..2, weight = ..3, month = ..4, price = ..5)
}

cocaineData <- x_(list(
	asRow("MA", 74,  3,  7,  180),
	asRow("NY", 83, 34, 10,  960),
	asRow("SC", 81, 47,  6, 1800),
	asRow("NY", 50, 27, 12, 1000),
	asRow("NY", 81,  1, 11,  100),
	asRow("FL", 57,  1,  8,  100),
	asRow("NJ", 47,  6,  5,  400),
	asRow("FL", 37, 52,  3, 1600),
	asRow("PA", 74,  2,  1,  200)
))

# 1. get and sort the state seisure frequencies

cocaineData $ xPluck("state") $ xTabulate() $ x_SortBy(xSecondOf)

'
list(
    list("MA", 1),
    list("SC", 1),
    list("NJ", 1),
    list("PA", 1),
    list("FL", 2),
    list("NY", 3))
'

# 2. group the seizures by state.
stateSeizures <- cocaineData $ xGroupBy(x. $ state)

# 3. get the largest intrastate seizures by price
largestStateSeizures <- stateSeizures $ xPluck('price') $ xMap(group := {
    xMaxBy(x. $ price, group)
})

'
list(
	list(state = "MA", potency = 74, weight = 3, month = 7,  price = 180),
	list(state = "NY", potency = 50, weight = 27, month = 12, price = 1000),
	list(state = "SC", potency = 81, weight = 47, month = 6,  price = 1800),
	list(state = "FL", potency = 37, weight = 5", month = 3,  price = 1600),
	list(state = "NJ", potency = 47, weight = 6,  month = 5,  price = 400),
	list(state = "PA", potency = 74, weight = 2,  month = 1,  price = 200)
)
'

# 4. get the average potency of the largest seizure

largestStateSeizures $ xPluck('potency') $ xTap(unlist %then% mean)

60.8
```

### Arrow is Expressive

* Programs are built like lego; stacking small,
uniquely-purposed functions into a larger program.

* Arrow is general enough to let you use the same functions for your
data reshaping and general purpose programming code.

* Arrow has a shorthand syntax for creating functions.

* Arrow implements jQuery-style method-chaining.

* Functions have variadic and non-variadic forms, cutting out all 'do.call' boilerplate.

* Arrow adds list-comprehensions, an expressive syntax for creating lists.

### Arrow is Consistent

* Functions don't discriminate between different types of vectors; lists, pairlists
and typed vectors are all interchangable.

* Arrow is very easy to debug, thanks to input validation and automatically summarising bad input.

* Arrow functions work perfectly with base R functions.

* Arrow uses consistent naming conventions.

### Arrow is Functional

* Every commonly used higher-order-function is included in Arrow, including but not limited to
map, fold, select, flatmap and iterate.

* Arrow includes several mathematical functions, like the set operations and
combinatoric functions.

* Arrow adds a big brother of **return( )** - **Return( )** - to make functions like fold
much more efficient.

* Function composition and partial application are encouraged as standard operations.

## Licensing

**Arrow** is released under the terms of the GNU General Public License version 3.

<img src="https://raw.githubusercontent.com/rgrannell1/arrow/develop/gpl3.png" height = "120"> </img>

## Versioning

All versions post-release will be compliant with the Semantic Versioning 2.0.0 standard.

http://semver.org/

## Authors

Ryan Grannell.
