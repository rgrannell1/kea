
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

message('x_(+)')

	over(val) +

	describe('the identity of x_(val) is always itself.') +
	when(
		True,
		x_(val) $ x_Identity() %is% val
	) +

	describe('x_(x_(val)) flattens.') +
	when(
		True,
		x_(x_(val)) %is% x_(val)
	) +

	run()
