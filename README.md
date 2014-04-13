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

```javascript
"-- Data From Hadley Wickham's https://github.com/hadley/data-stride"

rawCocaineData <-
"
state potency weight month price
MA    74       3      7     180 
NY    83      34     10     960
SC    81      47      6    1800 
NY    50      27     12    1000 
NY    81       1     11     100 
FL    57       1      8     100 
NJ    47       6      5     400 
FL    37      52      3    1600 
PA    74       2      1     200
"

keys <- x_(rawCocaineData) $ xToLines() $ xTake(1) $ x_ToWords()
nameRow <- xPartial(xAddKeys, list(keys))

cocaineData <-
    x_(rawCocaineData) $ xToLines() $ xDrop(1) $ 
    xMap(xToWords %then% as.list %then% nameRow)
    
"-- 1. get and sort the state seisure frequencies"

cocaineData $ xAtCol(1) $ xTabulate() $ x_SortBy(xSecondOf)

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
stateSeizures $ xAtCol(2) $ x_Map(group := {
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
```

### Arrow is Expressive

* Programs are built in a lego-like manner; stacking small,
uniquely-purposed functions into a larger program.

* Like Ruby and JavaScript, Arrow has a shorthand syntax 	for creating functions.

* Arrow implements jQuery-like chaining methods without using R's bulkier
object-orientation systems. These methods are just as usable as those in
Python or JavaScript. This syntax is handy for interactive programming.

* Arrow uses consistent naming conventions to make its functions more predictable.

* Almost every function has a variadic and non-variadic form, to
reduce boilerplate code.

* Arrow adds collection-comprehensions; an expressive syntax
for creating collections.

### Arrow is Consistent

* Functions don't discriminate between different types of vectors; lists, pairlists
and typed vectors are all interchangable.

* Extensive effort was made to make sure that Arrow never throws
a cryptic error - arguments are always explicitly checked, and the precise
cause of the error is always given. Error messages are also coloured for readability.

* Arrow is interoperable with base R functions.

### Arrow is Functional

* Arrow implements all the standard higher-order functions,
as well as set operations and combinatoric functions.

* Several variants of the most common functionals are implemented,
including several flavours of map and fold.

* A new control statement - the ```Return( )``` function - can
be used to improve the efficiency of functional code.

* Partial application and function composition are encouraged
as standard operations.

## Authors

Ryan Grannell.

## Licensing

**Arrow** is released under the terms of the GNU General Public License version 3.

<img src="gpl3.png" height = "180"> </img>


## Versioning

All versions post-release will be compliant with the Semantic Versioning 2.0.0 standard.

http://semver.org/

