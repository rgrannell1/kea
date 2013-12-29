

message('testing that methods that should be implemented are implemented.')

	# Automated Test 1
	#
	# Checking the arrow methods programmatically, to ensure that every method has
	# an unchaining version, and if variadic methods are implemented then they
	# have unchaining versions to.


	exports <- ls('package:arrow')

	arrow <- list(
		functions =
			exports[grepl('^x[A-Z]', exports)]

	)

	x_proto_methods <- list(
		any =
			ls(arrow ::: x_any_proto),
		fn =
			ls(arrow ::: x_fn_proto),
		coll =
			ls(arrow ::: x_coll_proto),
		data_frame =
			ls(arrow ::: x_data_frame_proto),
		matrix =
			ls(arrow ::: x_matrix_proto)
	)

	base_methods_names <- gsub('[.]{3}', '', arrow$functions)

	method_types <- list(
		as_normal =
			xI,
		as_variadic =
			base := {
				xUnchars...(base, '...')
			},
		as_unchaining =
			base := {
				xUnchars...('x_', xSubString(base, -1))
			},
		as_variadic_unchaining =
			base := {
			xUnchars...('x_', xSubString(base, -1), '...')
		}
	)


	make_method_types <- xJuxtapose(method_types)

	expected_methods <- xMap(make_method_types, base_methods_names)

	# FIX MAKE PROTO SPECIFIC.

	exceptions <- list(
		normal =
			c('xJuxtapose'),
		unchaining =
			c('xJuxtapose'),
		both_variadics =
			c()
	)

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
						(method %in% matches) || (method %in% exceptions$normal), invoking_call,
						wail$normal_form_missing(
							method, proto_name, matches))

					# every xMethod will have an unchaining x_Method

					assert(
						(forms$as_unchaining %in% matches) ||
						(method %in% exceptions$unchaining), invoking_call,
						wail$unchaining_form_missing(
							method, proto_name, matches))

					variadic_match <- forms$as_variadic %in% matches
					variadic_unchaining_match <- forms$as_variadic_unchaining %in% matches

					# either neither xMethod... or x_Method... is present, or they both are.

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

