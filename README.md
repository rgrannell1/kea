Arrow < v0.1.0 [![Build Status](https://travis-ci.org/rgrannell1/arrow.png)](https://travis-ci.org/rgrannell1/arrow)
-----------------------------------


## What is Arrow?

Arrow makes R the functional language it was born to be.

Arrow is an opinionated toolset of more than one hundred functions from languages
like Haskell, JavaScript, and functions designed just for R. These
include familiar functions like Map, Select, Reduce and Compose, as
well as many functions for working with na values, reshaping collections,
composing functions and manipulating strings.

I hope you enjoy using Arrow as much as I've enjoyed making it.
If you'd like to contribute with feedback, bug-reports or code
you can go to the Arrow [github repository](https://github.com/rgrannell1/arrow).
Any feedback is appreciated.

For library documentation and tutorials head to
[http://rgrannell1.github.io/arrow/](http://rgrannell1.github.io/arrow/).


## What Does Arrow Look Like?

Arrow can be written using normal functions or with chaining methods. I prefer the latter. Chaining 
methods take an initial piece of data and apply successive transformation to it. 
This is a very natural way of expressing data munging or preprocessing.

```javascript
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
    xMaxBy(row := as.numeric(row $ price) , group)  
})

'
list(
	list(state = "MA", potency = "74", weight = "3",  month = "7",  price = "180"),
	list(state = "NY", potency = "50", weight = "27", month = "12", price = "1000"), 
	list(state = "SC", potency = "81", weight = "47", month = "6",  price = "1800"), 
	list(state = "FL", potency = "37", weight = "52", month = "3",  price = "1600"), 
	list(state = "NJ", potency = "47", weight = "6",  month = "5",  price = "400"), 
	list(state = "PA", potency = "74", weight = "2",  month = "1",  price = "200")
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

* Arrow implements all the standard higher-order functions,
as well as set operations and combinatoric functions.

* Several variants of the most common functionals are implemented,
including several flavours of map and fold.

* Arrow adds a big brother of *return()* - *Return()* - to allow breaking out of 
higher-order functions like fold.

* Function composition and partial application are encouraged as standard operations.

## Licensing

**Arrow** is released under the terms of the GNU General Public License version 3.

<img src="gpl3.png" height = "120"> </img>


## Versioning

All versions post-release will be compliant with the Semantic Versioning 2.0.0 standard.

http://semver.org/

## Authors

Ryan Grannell.
