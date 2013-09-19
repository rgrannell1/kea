
xAsFunction <- function (coll) {
	# Collection any -> (number -> any)
	# enclose a collection in a function, and
	# allow access by supplying indices.

	pcall <- sys.call()

	assert(
		is.vector(coll) || is.pairlist(coll), pcall)

	function (...) {

		nums <- list(...)

		assert(
			is.numeric(nums) && all(round(nums) == nums), pcall)

		as.list(coll[nums])
	}
}
