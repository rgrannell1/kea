
feature list;
	
	- generic for pairlists, lists and vectors
	
	- typesafe: guaranteed to produce output of one type. checks
		that input functions are meeting their type signature.
	
	- functions can be partially applied, with function call 
		semantics otherwise identical to normal R.
	
	- consistent parameter naming convention.
	
	- idiomatic; trinary logic for input predicate functions,
		and functions that can be vectorised without buggering the 
		type signiture should be.

	- paired functions: functions should have inverses or counterparts,
		with semi-obvious names.

	- x-prefix for IDE search

	- chaining syntax, without partially applied functions.
		x_(a) monad constructor;
		method to pass a into a function.

	- groupby, sortby, orderby, countby operators.

	- logical quantifiers and cartesian products.

	- if a function is non-deterministic with respect to length,
	return vector(0). If it fixed with respect to length, return a single value.

	< character > needs two corner cases:
		<    > or
		< '' >

	so functions that require a string really require 
		f( length-one < string > )
		Vector string -> ...

		xMaybeFirst
			[ ] = [ ]
			[ 1 ] = 12

	Anywhere a vector is required, the empty vector should be handled.

	should xAll -> logical(0)?

Partition Methods

divides a set into mutually exclusive sets of elements.
disjoint union.

	partition a set by a unary predicate function.
	partition a set by a binary equivelance operator.
	partition a set by a the output of a unary function.

xPartition <- function (pred, coll) {
	# binary -> [ [any] ]	
	# equivelance relation
}

xPartitionWith <- function () {
	# pred -> [ [any] ]
	# use predicate to match.


}

xPartitionBy <- function () {
	# unary 
	# use output.
}