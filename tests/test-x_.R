
kea ::: load_test_dependencies(environment())

unit_test('x_(+)')

	over(val) +

	it('the identity of x_(val) is always itself.') +
	holdsWhen(
		True,
		x_(val) $ x_Identity() %is% val
	) +

	it('x_(x_(val)) flattens.') +
	holdsWhen(
		True,
		x_(x_(val)) %is% x_(val)
	) +

	it('x_ returns a kea object') +
	holdsWhen(
		True,
		'kea' %is_in% class(x_(val))
	) +

	run()
