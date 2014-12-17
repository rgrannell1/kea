
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
		is.function(fn) || is.symbol(fn) || (is.character(fn) && length(fn) == 1 && exists(fn)),

		id $ fn(fn)
	) +

	run()
