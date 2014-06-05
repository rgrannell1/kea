
arrow ::: load_test_dependencies(environment())
is_collection <- arrow ::: is_collection

message('x_(+)')

	over(val) +
	describe('the identity of x_(val) is always itself.') +
	when(
		True,
		x_(val) $ x_Identity() %equals% val
	) +
	run()

	over(val) +
	describe('x_(x_(val)) flattens.') +
	when(
		True,
		x_(x_(val)) %equals% x_(val)
	) +
	run()
