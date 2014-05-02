Arrow 0.3.0 [![Build Status](https://travis-ci.org/rgrannell1/arrow.png)](https://travis-ci.org/rgrannell1/arrow)
-----------------------------------

> *'The enjoyment of one's tools is an essential ingredient of successful work.'* -- Donald E. Knuth

## What is Arrow?

Arrow makes R the functional language it was born to be.

R can be an uncooperative language. Every function has its own way of
handling missing values or naming its parametres. As a result, piecing together
individual functions to make a useful program can be difficult and requires a
lot of boilerplate code.

Arrow is an expressive composible foundation for programming in R. It implements all
the common higher-order functions - Map, Fold, GroupBy and so on - as well as any reshaping
operation you might want to perform on a collection. It has simple IO functions that let
you read and write to files with minimal overhead, and the set and combinatoric functions
you need for more advanced algorithms.

It aims to be completely consistent and above all else user-friendly. Arrow functions
rarely have more than two parametres, so functions will just work without configuration.

For library documentation and tutorials head to
[http://rgrannell1.github.io/arrow/](http://rgrannell1.github.io/arrow/).

## What Does Arrow Look Like?

Arrow can be written using normal functions or with chaining methods. I prefer the latter. Chaining
methods take an initial piece of data and apply successive transformation to it.
This is a very natural way of expressing data munging or reshaping
```r
"-- Data From Hadley Wickham's https://github.com/hadley/data-stride"

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

"-- 1. get and sort the state seisure frequencies"

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

"-- 2. group the seizures by state."
stateSeizures <- cocaineData $ xGroupBy(x. $ state)

"-- 3. get the largest intrastate seizures by price"
largestStateSeizures <- stateSeizures $ xPluck('price') $ xMap(group := {
    xMaxBy(x. $ price, group)
})

'
list(
	list(state = "MA", potency = 74, weight = 3", month = 7,  price = 180),
	list(state = "NY", potency = 50, weight = 27, month = 12, price = 1000),
	list(state = "SC", potency = 81, weight = 47, month = 6,  price = 1800),
	list(state = "FL", potency = 37, weight = 5", month = 3,  price = 1600),
	list(state = "NJ", potency = 47, weight = 6,  month = 5,  price = 400),
	list(state = "PA", potency = 74, weight = 2,  month = 1,  price = 200)
)
'

"-- 4. get the average potency of the largest seizure"

largestStateSeizures $ xPluck('potency') $ xTap(unlist %then% mean)

60.8
```

### Arrow is Expressive

* Programs are built in a lego-like manner; stacking small,
uniquely-purposed functions into a larger program.

* Arrow has a shorthand syntax for creating functions.

* Arrow implements jQuery-style method-chaining.

* Most functions have variadic and non-variadic forms; this significantly reduces
'do.call' boilerplate.

* Arrow adds list-comprehensions; an expressive syntax for creating lists.

### Arrow is Consistent

* Functions don't discriminate between different types of vectors; lists, pairlists
and typed vectors are all interchangable.

* Extensive effort was made to make sure that Arrow never throws
a cryptic error - arguments are always explicitly checked, and the precise
cause of the error is always given. Error messages are also coloured for readability.

* Arrow is interoperable with base R functions.

* Arrow follows careful thought-out naming conventions.

### Arrow is Functional

* Every commonly used higher-order-function is included in Arrow, not limited to but including
map, fold, select, flatmap and iterate.

* Arrow includes several mathematical functions, like the set operations and
combinatoric functions.

* Arrow adds a big brother of **return()** - **Return()** - to make functions like fold
much more efficient.

* Function composition and partial application are encouraged as standard operations.

## Licensing

**Arrow** is released under the terms of the GNU General Public License version 3.

<img src="gpl3.png" height = "120"> </img>

## Versioning

All versions post-release will be compliant with the Semantic Versioning 2.0.0 standard.

http://semver.org/

## Authors

Ryan Grannell.
