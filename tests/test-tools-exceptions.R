
kea ::: load_test_dependencies(environment())

	id <- list()

	id $ coll = MakeFun(function (coll) {coll})
	id $ fn   = MakeFun(function (fn) fn)
	id $ str  = MakeFun(function (str) str)
	id $ ims  = MakeFun(function (ims) ims)

int_test('collection handling')






int_test('function handling')

	over(fn) +

	it('works for every type of function') +
	worksWhen(
		is.function(fn),

		id $ fn(fn)
	) +

	it('throws a type error otherwise') +
	holdsWhen(
		!is.function(fn) && !is.symbol(fn) && !is.character(fn),

		inherits(grasp(id $ fn(fn)), 'type_error'),
		grepl('type_error', grasp(id $ fn(fn)) $ message )
	) +

	run()





int_test('string handling')

	over(str) +

	it('works for strings') +
	worksWhen(
		is_character(str) && !is_na(str) && length(str) <= 1,

		id $ str(str)
	) +

	it('throws a type error otherwise') +
	holdsWhen(
		!is_character(str),

		inherits(grasp(id $ str(str)), 'type_error'),
		grepl('type_error', grasp(id $ str(str)) $ message )
	) +

	run()





int_test('always returns kea-specific errors. Should catch any unhandled errors.')

	over(val1) +

	it('never throws a non-kea error.') +

	holdsWhen(
		throws_error(xLenOf(val1)),

		throws_kea_error(xLenOf(val1)),
		throws_kea_error(xToWords(val1)),
		throws_kea_error(xRepeat(val1, val2)),
		throws_kea_error(xFromWords(val1)),
		throws_kea_error(xLift(val1))
	) +

	run(60)
