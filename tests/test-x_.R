
kiwi ::: load_test_dependencies(environment())

message('x_(+)')

	over(val) +

	describe('the identity of x_(val) is always itself.') +
	holdsWhen(
		True,
		x_(val) $ x_Identity() %is% val
	) +

	describe('x_(x_(val)) flattens.') +
	holdsWhen(
		True,
		x_(x_(val)) %is% x_(val)
	) +

	run()
