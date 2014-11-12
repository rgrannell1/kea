
kea ::: load_test_dependencies()

message("xUnspread")

	over(fn) +

	describe('all functions become unary') +
	holdsWhen(
		suchThat $ is_function(fn),

		length( formals(xUnspread(fn) )) == 1
	) +

	run()

