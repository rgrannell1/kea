
Kiwi 0.15.0 [![Build Status](https://travis-ci.org/rgrannell1/kiwi.png)](https://travis-ci.org/rgrannell1/kiwi)
-----------------------------------

> *'By relieving the brain of all unnecessary work, a good notation sets it free to concentrate on more advanced problems, and, in effect, increases the mental power of the race.' -- Alfred N. Whitehead*

### Public Release: ~1 August 2014

Kiwi makes R an effective language for functional programming.

### Installation

Kiwi isn't (and probably won't be) on CRAN. This is to allow for frequent updates; it
is considered bad etiquette to update a CRAN package more than once a month.

```js
install.packages("devtools")
install_github("kiwi", "rgrannell1", ref = "releases")
```

## What is Kiwi?

Functional programming has become commonplace in languages like JavaScript
and Python, but R is conspicuously lacking such a library. Kiwi is a functional
library for general-purpose programming in R. It adds all the common higher-order
functions (Map, Fold, Compose, ...) and functions taken from
set theory and combinatorics. Kiwi also exploits R's flexibility to add
kiwi functions, methods, wildcards & list-comprehensions to the language.

For library documentation and tutorials head to
[http://rgrannell1.github.io/kiwi/](http://rgrannell1.github.io/kiwi/).




## What Does Kiwi Look Like?

First, a table of Kiwi's (optional) new syntax.

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

With that out the way, here is a simple use of Kiwi to examine cocaine seizure data.

```js
# // Data From Hadley Wickham's https://github.com/hadley/data-stride

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

#### Kiwi is Expressive

It is easier to write a sentence from left to right than from the middle out; writing programs
as chains of functions is similarily natural.

Kiwi code is *compositional*; to create an kiwi program you chain functions
together into a pipeline that takes your input and transforms it in multiple
steps. You don't need to worry about odd output of one function suddenly killing
the next, as corner cases are consistent within Kiwi.

```js
# method example
```

R was an early language to include anonymous, first-class functions. They are a common part of the
language, and are widely used with functions like `lapply` and `Filter`. Unfortunately R's function's
definitions are verbose. Arrow-functions are terser than normal function expressions.

```js
# -- generate the pairs [[a, A], [b, B], ...]
x_(letters) $ xMap(letter := {
	list(letter, toupper(letter))
})
```

You could also create this collection using collection-comprehensions; syntax sugar for creating
new collections by filtering, joining & transforming existing collections.

```js
xList[ list(l, toupper(l)), l <- letters ]
```

There are two approaches to making a function that can take a variable number of arguments. The first is
to use the ellipsis parametre (...), which gathers up any arguments passed to a function. The second is to
simply pass one list of arguments to the function. Both approaches have their merits and pitfalls.

The first approach - using ellipsis - is less verbose, but less flexible. The second approach - using a
list or arguments- is conversely more flexible, but more verbose.

```js
xJoin_(list(1, 2), list(3, 4))
```

### Kiwi is Expressive

* Programs are built like lego; stacking small,
uniquely-purposed functions into a larger program.

* Kiwi is general enough to let you use the same functions for your
data reshaping and general purpose programming code.

* Kiwi has a shorthand syntax for creating functions.

* Kiwi implements jQuery-style method-chaining.

* Functions have variadic and non-variadic forms, cutting out all 'do.call' boilerplate.

* Kiwi adds list-comprehensions, an expressive syntax for creating lists.

### Kiwi is Consistent

* Functions don't discriminate between different types of vectors; lists, pairlists
and typed vectors are all interchangable.

* Kiwi is very easy to debug, thanks to input validation and automatically summarising bad input.

* Kiwi functions work perfectly with base R functions.

* Kiwi uses consistent naming conventions.

### Kiwi is Functional

* Every commonly used higher-order-function is included in Kiwi, including but not limited to
map, fold, select, flatmap and iterate.

* Kiwi includes several mathematical functions, like the set operations and
combinatoric functions.

* Kiwi adds a big brother of **return( )** - **Return( )** - to make functions like fold
much more efficient.

* Function composition and partial application are encouraged as standard operations.

## Licensing

**Kiwi** is released under the terms of the GNU General Public License version 3.

<img src="https://raw.githubusercontent.com/rgrannell1/kiwi/develop/gpl3.png" height = "120"> </img>

## Versioning

All versions post-release will be compliant with the Semantic Versioning 2.0.0 standard.

http://semver.org/

## Authors

Ryan Grannell.
