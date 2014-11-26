
kea ::: load_test_dependencies(environment())

unit_test("xParamsOf")

	over(fn) +

	it("always returns character") +
	holdsWhen(
		suchThat $ is_function(fn),

		is.character(xParamsOf(fn))
	) +

	it("works for non-primitive functions") +
	holdsWhen(
		suchThat $ is_closure(fn),

		( xParamsOf(fn) %is% names(formals(fn)) ) ||
		( xParamsOf(fn) %is% character(0) )
	) +

	run()
