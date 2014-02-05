

xList <- structure(NULL, class = 'list_builder')

'[.list_builder' <- function (x, ...) {

	exprs <- eval( substitute(alist(...)) )

	parent_frame <- parent.frame()

	if (length(exprs) == 0) {
		list()
	} else {

		components <- local({

			is_binding <- function (expr) {
				length(expr) == 3 && expr[[1]] == '<-'
			}
			 
			this <- list()
			binding_indices <- 
				which( vapply(exprs, is_binding, logical(1)) )

			stopifnot(1 %!in% binding_indices)

			bindings <- exprs[binding_indices]

			this$yield <- 
				exprs[[1]]

			this$predicates <- 
				exprs[seq_along(exprs) %!in% c(1, binding_indices)]
			
			this$variables <-
				vapply(bindings, function (expr) paste0( expr[[2]] ), character(1))

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

			this$predicates <- lapply(components$predicates, function (expr) {
				parametreise(expr, components$variables)
			})

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

			all_matches <- True
			candidate <- arguments$tuples[[ith]]

			for (pred in parametreised$predicates) {

				this_is_match <- do.call(pred, candidate, envir = parent_frame)
				all_matches <- all_matches && this_is_match

			}

			if (all_matches) {
				results <- c(results, list(candidate))
			}
		}

		lapply(results, function (result) {
			do.call(parametreised$yield, result, envir = parent_frame)
		})
	}
}

xList[a*b, a <- 1:10, b <- 1:10, a + b == a^b]

