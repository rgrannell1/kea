Arrow pre-v0.1.0
-----------------------------------

<img src="logo.png" height = "180"> </img>

**DISCLAIMER: Arrow is in heavy development and is highly liable to change.**


Arrow is a toolset of more than one hundred functions from languages
like Haskell, Javascript, and functions designed just for R. These
include familiar functions like Map, Select, Reduce and Compose, as
well as many functions for working with na values, reshaping collections,
composing functions and manipulating strings.

I hope you enjoy using Arrow as much as I've enjoyed making it! To
contribute with feedback, bug-reports or code go to the Arrow
[github repository](https://github.com/rgrannell1/arrow). Any feedback is
appreciated.

### What Arrow code looks like.

```r
# how many primitive functions are there in R?

xPoll(
	fn_name := {
		fn <- get(fn_name)
		is.function(fn) && is.primitive(fn)
	},
	ls('package:base')
)

178
```

### Arrow is Expressive

* Programs are built in a lego-like manner; stacking small,
uniquely-purposed functions into a larger program.

* Like Ruby and Javascript, Arrow has a shorthand syntax
for creating functions to R.

* Arrow implements jQuery-like chaining methods without using R's bulkier
object-orientation systems. These methods are just as usable as those in
Python or Javascript. This syntax is handy for interactive programming.

* Arrow uses small but powerful naming conventions to make its
programs optimally easy to write.

* Almost every function has a variadic and non-variadic form, to
reduce boilerplate code.

### Arrow is Consistent

* Functions don't discriminate between different types of vectors; lists, pairlists
and typed vectors are all interchangable.

* Extensive effort was made to make sure that Arrow never throws
a cryptic error - arguments are always explicitly checked, and the precise
cause of the error is always given.

* Arrow is interoperable with base R functions.

### Arrow is Functional

* Arrow implements all the standard higher-order functions,
as well as dozens of collection-reshaping functions.

* Several variants of the most common functionals are implemented,
including several flavours of map and fold.

* A new control statement - the ```Return( )``` function - can
be used to improve the expressiveness of functional code.

* Partial application and function composition are encouraged
as standard operations.

## 5 Authors

Ryan Grannell.

## 6 Licensing

**Arrow** is released under the terms of the GNU General Public License version 3.

<img src="gpl3.png" height = "180"> </img>

