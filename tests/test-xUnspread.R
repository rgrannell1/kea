
kiwi ::: load_test_dependencies(environment())

message("xUnspread")

	over(fn) +

	describe('all functions become unary') +
	holdsWhen(
		is.function(fn),
		length( formals(xUnspread(fn) )) == 1
	) +

	run()

