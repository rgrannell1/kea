
kea ::: load_test_dependencies()

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

	describe('x_ returns a kea object') +
	holdsWhen(
		True,
		'kea' %is_in% class(x_(val))
	) +

	run()
