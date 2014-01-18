
message('Arrow Methods')
message('test that every method has an unchaining version.')

	exports <- ls('package:arrow')

	arrow <- list(
		functions =
			exports[grepl('^x[A-Z]', exports)] )

	x_proto_methods <- list(
		xany =
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
			xI,
		as_variadic =
			base := {
				xFromChars...(base, '...')
			},
		as_unchaining =
			base := {
				xFromChars...('x_', xSubstring(base, -1))
			},
		as_variadic_unchaining =
			base := {
			xFromChars...('x_', xSubstring(base, -1), '...')
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
			c()
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

					assert(
						(method %in% matches) ||
						(method %in% exceptions$normal), invoking_call,
						wail$normal_form_missing(
							method, proto_name, matches))

					# every xMethod will have an unchaining x_Method

					assert(
						(forms$as_unchaining %in% matches) ||
						(method %in% exceptions$unchaining), invoking_call,
						wail$unchaining_form_missing(
							method, proto_name, matches))

					variadic_match <-
						xIsMember(forms$as_variadic, matches)
					variadic_unchaining_match <-
						xIsMember(forms$as_variadic_unchaining, matches)

					# either neither xMethod... or x_Method...
					# is present, or they both are.

					if (variadic_match || variadic_unchaining_match) {

						assert(
							variadic_match, invoking_call,
							wail$variadic_form_missing(
								method, proto_name, matches))

						assert(
							variadic_unchaining_match, invoking_call,
							wail$variadic_unchaining_form_missing(
								method, proto_name, matches))

					}
				}
			}

		},
		xSetProd...(
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

				assert(
					proto_name %!in% expected_proto || (method %in% proto) ||
					( method %in% implement_exception[[proto_name]] ),
					invoking_call,
					wail$method_not_in_proto(method, proto_name))

			}
		},
		xSetProd...(
			base_methods_names,
			names(x_proto_methods))
	)
