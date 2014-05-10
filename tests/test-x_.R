
forall <- arrow:::forall
test_cases <- arrow:::test_cases
match_fn <- arrow:::match_fn
'%!in%' <- arrow:::'%!in%'
wail <- arrow:::wail

require(arrow)

message('Arrow Methods')
message('test that every method has an unchaining version.')

	exports <- ls(getNamespace('arrow'))

	arrow <- list(
		functions =
			exports[grepl('^x[A-Z]', exports)] )

	x_proto_methods <- list(
		xAnyOf =
			ls(arrow ::: x_any_proto),
		fn =
			ls(arrow ::: x_fn_proto),
		coll =
			ls(arrow ::: x_coll_proto),
		data_frame =
			ls(arrow ::: x_data_frame_proto),
		matrix =
			ls(arrow ::: x_matrix_proto) )

	base_methods_names <- gsub('[.]{3}', '', arrow$functions)

	method_types <- list(
		as_normal =
			identity,
		as_variadic =
			function (base) {
				xFromChars_(base, '_')
			},
		as_unchaining =
			function (base) {
				xFromChars_('x_', xSliceString(base, -1))
			},
		as_variadic_unchaining =
			function (base) {
			xFromChars_('x_', xSliceString(base, -1), '_')
		}
	)

	make_method_types <- xJuxtapose(method_types)
	expected_methods <- xMap(make_method_types, base_methods_names)

	# reasons for exceptions:
	#    multi-function functionals cannot have normal
	#    methods, as they're functions
	#    on functions, not collections of functions.

	# not proto specific; could be improved.

	exceptions <- list(
		normal =
			c('xJuxtapose', 'xCompose', 'xLift'),
		unchaining =
			c('xJuxtapose', 'xCompose', 'xLift'),
		both_variadics =
			c('xExplode')
	)

	sink <-
	xMapply(
		(method : proto_name) := {

			invoking_call = 'unit test 1'

			proto <- x_proto_methods[[proto_name]]

			if (method != 'x') {

				forms <- make_method_types(method)

				matches <- xSelect(
					proto_method := {
						proto_method %in% forms
					},
					proto
				)

				if (length(matches) > 0) {

					# every xMethod should have an xMethod

					if (method %!in% matches && method %!in% exceptions$normal) {
						stop(
							wail$normal_form_missing(method, proto_name, matches))

					}

					# every xMethod will have an unchaining x_Method

					if (forms$as_unchaining %!in% matches && method %!in% exceptions$unchaining) {
						stop(wail$unchaining_form_missing(
							method, proto_name, matches))
					}

					variadic_match <-
						xIsMember(forms$as_variadic, matches)

					variadic_unchaining_match <-
						xIsMember(forms$as_variadic_unchaining, matches)

					# either neither xMethod_ or x_Method_
					# is present, or they both are.

					if (variadic_match || variadic_unchaining_match) {

						if (!variadic_match) {
							stop(wail$variadic_form_missing(
								method, proto_name, matches))
						}

						if (!variadic_unchaining_match) {

							stop(wail$variadic_unchaining_form_missing(
								method, proto_name, matches))
						}

					}
				}
			}

		},
		xProdSetOf_(
			base_methods_names,
			names(x_proto_methods))
	)





message('test that every function has methods.')

	guess_prototypes <- params := {

		guesses <- c(
			coll =
				'coll',
			colls =
				'coll',
			num =
				'coll',
			nums =
				'coll',
			rexp =
				'coll',
			fn =
				'fn',
			pred =
				'fn',
			str =
				'coll',
			strs =
				'colls'
		)

		unique(guesses[params])
	}

	implement_exception <- list(
		any =
			c(),
		fn =
			c(),
		coll =
			c('xAsVal', 'xAsVar', 'xIsVal', 'xVal'),
		data_frame =
			c(),
		matrix =
			c()
	)

	sink <-
	xMapply(
		(method : proto_name) := {

			invoking_call = 'unit test 2'

			proto <- x_proto_methods[[proto_name]]

			if (method != 'x') {

				params <- xParamsOf(match_fn(method))

				# try to guess what prototype the function should belong to
				# based on the (rather systematic) parametre names.

				expected_proto <- guess_prototypes(params)

				# if the method is in a non-expected class, complain.

				if (proto_name %in% expected_proto && (method %!in% proto)) {
					stop(
						wail$method_not_in_proto(method, proto_name))
				}

			}
		},
		xProdSetOf_(
			base_methods_names,
			names(x_proto_methods))
	)

message('test that the constructor works for each data type')

	forall(
		"collections work",
		test_cases$collection,
		x_(coll) $ x_() %equals% coll
	)

	forall(
		"matrices work",
		test_cases$collection,
		x_(as.matrix(coll)) $ x_() %equals% as.matrix(coll)
	)

	forall(
		"data.frames work",
		test_cases$collection,
		x_(as.data.frame(coll)) $ x_() %equals% as.data.frame(coll)
	)

	forall(
		"factors work",
		test_cases$collection,
		x_(as.factor(coll)) $ x_() %equals% as.factor(coll)
	)

	forall(
		"functions work",
		test_cases$base_function,
		x_(fn) $ x_() %equals% fn
	)
