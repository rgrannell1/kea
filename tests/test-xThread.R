
kea ::: load_test_dependencies(environment())

unit_test("xThread")

	over(val) +

	it('returns a value given no functions') +
	holdsWhen(
		True,

		xThread(val, list()) %is% val
	) +

	it('returns a value given only identity functions') +
	holdsWhen(
		True,

		xThread(val, list(identity))           %is% val,
		xThread(val, list(identity, identity)) %is% val
	) +

	run()
