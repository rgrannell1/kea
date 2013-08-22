
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
