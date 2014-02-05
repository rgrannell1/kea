
xList <- structure(NULL, class = 'list_builder')

print.list_builder <- function (x, ...) {
	cat("[ the xList object ]\n")
}

'[.list_builder' <- function (x, ...) {

	exprs <- eval( substitute(alist(...)) )
	parent_frame <- parent.frame()

	invoking_call <- match.call()[-1]

	if (length(exprs) == 0) {
		list()
	} else {

		components <- local({
			 
			this <- list()
			binding_indices <- 
				which( vapply(exprs, function (expr) {

					length(expr) == 3 && expr[[1]] == '<-'

				}, logical(1)) )

			demand $ must_have_yield(binding_indices, invoking_call)
			demand $ must_be_unnamed(exprs, invoking_call)

			bindings <- exprs[binding_indices]

			this$yield <- 
				exprs[[1]]

			is_predicated <- 
				length(exprs) %!in% binding_indices && length(exprs) > 1

			this$predicate <- 
				if (is_predicated) {
					exprs[[ length(exprs) ]]					
				} else {
					True
				}

			if (is_predicated) {

				demand $ must_all_be_matched(
					c(1, binding_indices, length(exprs)), exprs, invoking_call)
			} else {
				demand $ must_all_be_matched(
					c(1, binding_indices), exprs, invoking_call)				
			}

			# check that all expressions are matched.

			this$variables <-
				vapply(bindings, function (expr) {
					paste0( expr[[2]] )
				}, character(1))

			this$values <-
				lapply(bindings, function (expr) {
					eval(expr[[3]], envir = parent_frame)
				})

			this
		})

		parametreised <- local({

			this <- list()

			parametreise <- function (expr, params) {
				# add the parametres to the functions

				fn <- do.call('function', list(
					as.pairlist(as_parametres(params)), 
					expr
				))
				environment(fn) <- parent_frame
				fn
			}

			this$yield <- parametreise(
				components$yield, components$variables)

			this$predicate <- parametreise(
				components$predicate, components$variables)

			this
		})

		arguments <- local({
			# take the set product of components$values

			this <- list()

			coll_lengths <- vapply(components$values, length, integer(1))

			if (length(components$values) == 0 || min(coll_lengths) == 0) {
				list()
			} else {

				modulo_iths <- function (num, mods) {

					assert(num <= prod(mods), invoking_call)
					as.numeric(arrayInd(num, .dim = mods))
				}

				tuples <- vector(mode = "list", prod(coll_lengths))

				for ( ith in seq_len(prod(coll_lengths)) ) {

					indices <- modulo_iths(ith, coll_lengths)

					tuples[[ith]] <- Map(
						function (coll_ith) {
							choice <- indices[coll_ith]
							components$values[[coll_ith]][[choice]]
						},
						seq_along(components$values))

				}

				this$tuples <- tuples
			}

			this
		})

		results <- list()

		for (ith in seq_along(arguments$tuples)) {

			candidate <- arguments$tuples[[ith]]
			is_match <- 
				do.call(
					parametreised$predicate, 
					candidate, 
					envir = parent_frame)

			if (isTRUE(is_match)) {
				results <- c(
					results, do.call(parametreised$yield, candidate, envir = parent_frame))
			}
		}
		as.list(results)
	}
}

xList[ list(x, y), x <- 1:10, y <- 1:10, x * y > 3 ]
