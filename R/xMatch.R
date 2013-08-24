
xMatch <- function (value, ...) {
	# any -> [any, any] ... -> any
	# a lightweight pattern-matching algorithm, 
	# based on the algorithm used by clojure's core.match

	pcall <- sys.call()
	lazy_args <- as.list( match.call(expand.dots = False) )[-1]

	require_a("arbitrary", value, pcall)
	require_a(list_of_length_two, lazy_args$..., pcall)

}
