
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

	local({

		fns <- Map(
			function (fn) {
				list( fn, getNamespace('kea')[[fn]] )
			},
			Filter(
				function (fn_name) {
					!any(fn_name == c('xRead', 'xWrite'))
				},
				ls('package:kea', pattern = '^x[A-Z]')
			)
		)


		lapply(fns, function (fn_info) {

			fn_name <- fn_info[[1]]
			fn      <- fn_info[[2]]

			FN      <- as.symbol(fn_name)

			number_of_params <- length(formals(fn))
			number_of_args   <- sample.int(number_of_params, 1)

			expr <- bquote({

				over(val) +

				it(paste('never throws a naked error for ', fn_name)) +

				holdsWhen(
					throws_error(do.call( .(FN), rep(list(val), .(number_of_args)) )),

					throws_kea_error(do.call( .(FN), rep(list(val), .(number_of_args)) ))
				) +

				run(5)

			})





			eval(expr)

		})

	})
