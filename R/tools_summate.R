
# ---------------------------------------------------------------------------
#
# summate
#
# Summate summarises properties of an object as a list.

summate <- local({
	# Returns a string of information about an input object.

	output_key_value_pairs <-
		function (coll) {

			# -- collapse the properties into a string.
			Reduce(
				'%+%',
				lapply(names(coll), function (name) {

					padded_name <- gettextf("%-16s", name %+% ":")

					padded_name %+%
					'    ' %+% paste0(coll[[name]], collapse = '') %+% '\n'

			}) )
		}

	profile <- Object()

	# --- A --- #
	# --- B --- #
	# --- C --- #

	profile$`character vector` <-
		function (obj) {

			traits <- list(
				length =
					length(obj),
				`no empty` =
					length(which(nchar(obj) == 0)),
				`any empty` =
					all(nchar(obj) == 0),
				`any empty` =
					any(nchar(obj) == 0),
				classes =
					deparse(class(obj))
			)

			"\n\n" %+% "The actual input was a character vector with these properties:" %+% "\n\n" %+%
			output_key_value_pairs(traits)

		}

	profile$closure <-
		function (obj) {

			traits <- list(
				`is primitive` =
					is.primitive(obj),
				arity =
					if (is.primitive(obj)) {
						length( head(as.list(args(obj)), -1) )
					} else {
						length(formals(obj))
					},
				classes =
					deparse(class(obj))
			)

			"\n\n" %+% "The actual input was a function with these properties:" %+% "\n\n" %+%
			output_key_value_pairs(traits)
		}

	# --- D --- #

	profile$`data frame` <-
		function (obj) {

			traits <- list(
				classes =
					deparse(class(obj))
			)

			"\n\n" %+% "The actual input was a data frame with these properties:" %+% "\n\n" %+%
			output_key_value_pairs(traits)

		}

	profile$default <-
		function (obj) {

			traits <- list(
				classes =
					deparse(class(obj))
			)

			"\n\n" %+% "The actual input was a value with these properties:" %+% "\n\n" %+%
			output_key_value_pairs(traits)

		}

	profile$`double vector` <-
		function (obj) {

			traits <- list(
				length =
					length(obj),
				`no positive` =
					length(which(obj[ !(is_na(obj)) ] > 0)),
				`no zero` =
					length(which(obj[ !(is_na(obj)) ] == 0)),
				`no negative` =
					length(which(obj[ !(is_na(obj)) ] < 0)),
				`no na` =
					length( which(is_na(obj)) ),
				`no nan` =
					length( which(is.nan(obj)) ),
				`no whole` =
					local({
						roundable <- obj[ !(is_na(obj) | is.nan(obj) | is.infinite(obj)) ]
						length(which(round(roundable) == roundable))
					}),
				`no infinite` =
					length( which(is.integer(obj)) ),
				classes =
					deparse(class(obj))
			)

			"\n\n" %+% "The actual input was a double vector with these properties:" %+% "\n\n" %+%
			output_key_value_pairs(traits)

		}

	# --- E --- #
	# --- F --- #

	profile$factor <-
		function (obj) {

			traits <- list(
				`ordered factor` =
					is.ordered(obj),
				levels =
					length(levels(obj)),
				length =
					length(obj),
				classes =
					deparse(class(obj))
			)

			"\n\n" %+% "The actual input was a factor with these properties:" %+% "\n\n" %+%
			output_key_value_pairs(traits)

		}

	# --- G --- #

	profile$`generic vector` <-
		function (obj) {

			traits <- list(
				length =
					length(obj),
				`is nested` =
					any( vapply(obj, function (x) {
						isTRUE(is.list(x) || is.pairlist(x))
					}, logical(1)) ),
				`all nested` =
					all( vapply(obj, function (x) {
						isTRUE(is.list(x) || is.pairlist(x))
					}, logical(1)) ),
				`any arrow objects` =
					any( vapply(obj, function (x) any(class(x) == 'arrow'), logical(1)) ),
				`all arrow objects` =
					all( vapply(obj, function (x) any(class(x) == 'arrow'), logical(1)) )
			)

			"\n\n" %+% "The actual input was a list or pairlist with these properties:" %+% "\n\n" %+%
			output_key_value_pairs(traits)

		}

	# --- H --- #
	# --- I --- #

	profile$`integer vector` <-
		function (obj) {

			traits <- list(
				length =
					length(obj),
				`no positive` =
					length(which(obj[ !(is_na(obj)) ] > 0)),
				`no zero` =
					length(which(obj[ !(is_na(obj)) ] == 0)),
				`no negative` =
					length(which(obj[ !(is_na(obj)) ] < 0)),
				`no na` =
					length( which(is_na(obj)) ),
				classes =
					deparse(class(obj))
			)

			"\n\n" %+% "The actual input was a integer vector with these properties:" %+% "\n\n" %+%
			output_key_value_pairs(traits)

		}

	# --- J --- #
	# --- K --- #
	# --- L --- #

	profile$`logical vector` <-
		function (obj) {

			traits <- list(
				length =
					length(obj),
				`no na` =
					length( which(is_na(obj)) ),
				`no true` =
					length(which(obj)),
				`no false` =
					length(which(!obj)),
				classes =
					deparse(class(obj))
			)


			"\n\n" %+% "The actual input was a logical vector with these properties:" %+% "\n\n" %+%
			output_key_value_pairs(traits)

		}

	# --- M --- #

	profile$matrix <-
		function (obj) {

			traits <- list(
				nrow =
					nrow(obj),
				ncol =
					ncol(obj),
				type =
					deparse(typeof(obj)),
				classes =
					deparse(class(obj))
			)

			"\n\n" %+% "The actual input was a matrix with these properties:" %+% "\n\n" %+%
			output_key_value_pairs(traits)

		}

	# --- N --- #

	profile$null <-
		function (obj) {

			"\n\n" %+% "The actual input was NULL" %+% "\n\n"
		}

	# --- O --- #
	# --- P --- #
	# --- Q --- #
	# --- R --- #

	profile$`raw vector` <-
		function (obj) {

			traits <- list(
				length =
					length(obj),
				classes =
					deparse(class(obj))
			)


			"\n\n" %+% "The actual input was a raw vector with these properties:" %+% "\n\n" %+%
			output_key_value_pairs(traits)

		}

	# --- S --- #
	# --- T --- #

	# -- U --- #
	# --- V --- #
	# --- W --- #
	# --- X --- #
	# --- Y --- #
	# --- Z --- #

	# the set of implemented object summaries.

	function (obj) {
		# return the appropriate string summary,
		# depending on the type of object supplied.

		response_pairs <- list(
			list(
				is.function,
				profile$closure),

			list(
				is.null,
				profile$null),

			list(
				is.factor,
				profile$factor),

			list(
				function (x) is.list(x) || is.pairlist(x),
				profile$`generic vector`),

			list(
				function (x) is.logical(x),
				profile$`logical vector`),
			list(
				function (x) is.raw(x),
				profile$`raw vector`),
			list(
				function (x) is.integer(x),
				profile$`integer vector`),
			list(
				function (x) is.double(x),
				profile$`double vector`),
			list(
				function (x) is.character(x),
				profile$`character vector`),

			list(
				is.matrix,
				profile$matrix )
		)

		for (pair in response_pairs) {

			test <- pair[[1]]
			response <- pair[[2]]

			if (test(obj)) {
				message <- paste0(response(obj), collapse = '')

				return (message)
			}
		}

	profile$default(obj)
	}
})
