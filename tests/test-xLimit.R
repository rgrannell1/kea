
forall <- arrow:::forall
test_cases <- arrow:::test_cases

require(arrow)


message("xLimit")

	forall(
		"limiting to infinity is indistinguisable",
		test_cases$positive_with_linear_function,
		{
			bool <- TRUE
			limited <- xLimit(fn, Inf)

			for (ith in seq_len(num)) {
				bool && (limited(num) == fn(num))
			}
			bool
		}
	)

	forall(
		"limiting to 0 is the null function",
		test_cases$positive_with_linear_function,
		{
			bool <- TRUE
			limited <- xLimit(identity, 0)

			for (ith in seq_len(num)) {
				bool && is.null(limited(num))
			}
			bool
		}
	)

	forall(
		"limiting returns null after the expected time",
		test_cases$positive_with_linear_function,
		{
			bool <- TRUE
			limited <- xLimit(identity, num)

			for (ith in seq_len(num + 1)) {
				limited(num)
			}
			is.null(limited(num))
		}
	)

