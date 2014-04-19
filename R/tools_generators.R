
"coll = (:numeric: [1...10]+)"

match_val_class = function (val_class) {
	# try to match val class against a premade case

	cases = list()

	cases $ `:lower:` =
		function () one_of(letters)

	cases $ `:upper:` =
		function () one_of(LETTERS)

	cases $ `:logfun:` =
		function () {
			one_of(list(
				function () True,
				function () False,
				function () Na
			))
		}

	cases $ `:boolfun:` =
		function () {
			one_of(list(
				function () True,
				function () False
			))
		}

	cases $ `:logical:` =
		function () {
			one_of(list(True, False, Na))
		}

	cases $ `:boolean:` =
		function () {
			one_of(list(True, False))
		}

	cases $ `:linearfun:` =
		function () {
			sd <- 20
			function (num) {
				abs(round(rnorm(1, 0, sd), 0)) + 1 * num
			}
		}

	cases $ `:baseprim:` = local({
		fns <- Filter(
			function (fn) {
				is.function(fn) && is.primitive(fn)
			},
			lapply(ls('package:base'), get)
		)

		function () {
			one_of(fns)
		}
	})

	cases $ `:basefun:` = local({

		fns <- Filter(
			function (fn) {
				is.function(fn) && !is.primitive(fn)
			},
			lapply(ls('package:base'), get)
		)

		function () {
			one_of(fns)
		}
	})

	cases $ `:posinf:` =
		function () Inf

	cases $ `:neginf:` =
		function () Inf

	cases $ `:integer:` =
		function () {
			sd <- 20
			function () {
				round(rnorm(1, 0, sd))
			}
		}

	if (val_class %in% names(cases)) {
		cases[[val_class]]
	} else {
		NULL
	}
}

match_range = function (range_class) {
	# match a range selector.

	cases = list()

	# match integer ranges
	# -10...12, 10...+20
	cases $	`[+-]*[0-9]+[.]^{3}[+-]*[0-9]+` =
		function () {

		}

	for (pattern in names(cases)) {
		is_match <- grep(pattern, range_class)

		if (is_match) {
			return( cases[[pattern]] )
		}
	}

	NULL
}
