
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message("xUnspread (+)")

	over(fn) +

	describe('all functions become unary') +
	when(
		is.function(fn),
		length( formals(xUnspread(fn) )) == 1
	) +

	run()

