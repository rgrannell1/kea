
#' xList[ ]
#'
#' Generate a list using set-builder notation.
#'
#' @details
#'    \bold{xList} allows for shorthand construction of lists using a form of set-builder notation.
#'    The notation typically looks like
#'
#'    \code{S = {2.x | x e 1..100, x^2 > 3}}
#'
#'    which reads "S is the set of numbers '2x' from the
#'    numbers 1...100, where x^2 is larger than three".
#'    In general the syntax is of the form
#'
#'    \code{S = {output expression, variable e set, predicate expression}}
#'
#'    Kea's syntax is similar
#'
#'    \code{S <- xList[ 2*x, x <- 1:10 x^2 > 3 ]}
#'
#'    \code{S <- xList[output expression, variable <- set, predicate expression]}
#'
#'    Collection comprehensions are a shorthand for several operations; taking
#'    the \bold{set product} of several collections, \bold{selecting} them based
#'    on a predicate, and \bold{mapping} a function over each result.
#'
#'    \bold{1 The output expression}
#'
#'    The output expression corresponds to the map operation mentioned above.
#'    The for comprehension
#'
#'    \code{xList[2*x, x <- 1:10]}
#'
#'    is loosely translated into normal kea code as
#'
#'    \code{xMap(x := x^2, 1:10)}
#'
#'    \bold{2 Variable bindings}
#'
#'    At least one variable must be bound for a non-empty list
#'    comprehension. Multiple variables may also be bound.
#'
#'    \code{xList[list(x, y), x <- 1:3, y <- 1:3]}
#'
#'    The above comprehension loosely translates as
#'
#'    \code{x_(xSetProd(1:3, 1:3)) $ xMap( xUnspread((x : y) := c(x, y)) )}
#'
#'    \bold{3 The predicate expression (optional)}
#'
#'    If the final expression given to a comprehension is not a binding
#'    expression it is treated as a predicate for selecting particular
#'    values. this is optional. If it isn't included no elements are filtered out.
#'
#'    \code{xList[c(a, b), a <- 1:3, b <- 1:3, a + b > 2]}
#'
#'    In the above pairs of a, b such that their sum is larger than
#'    two are returned.
#'
#'
#' @section Corner Cases:
#'     Returns the empty list when no arguments are given.
#'
#' @template
#'    Variadic
#'
#' @example
#'    inst/examples/example-xList.R
#'
#' @rdname xList
#' @export

xList <- structure(
	function (...) {

		invoking_call <- sys.call()

		message <-
			"comprehension objects cannot be invoked as a " %+%
			"function: they must be invoked with square brackets ( [] )"

		throw_kea_error(invoking_call, message)
	},
	class = 'xlist_builder'
)


#' @export

print.xlist_builder <- function (x, ...) {
	cat("[ the xList object ]\n")
}


#' @export

`[.xlist_builder` <- function (x, ...) {

	exprs <- eval( substitute(alist(...)) )
	parent_frame <- parent.frame()

	# -- grab the named unevaluated parametres
	invoking_call <- match.call()[-1]

	# -- no args; just return list()
	if ( identical(exprs[[1]], quote(expr=)) ) {
		return(list())
	}

	components <- local({

		self <- list()

		# -- which expressions match any <- any?
		binding_indices <-
			which( vapply(exprs, function (expr) {

				length(expr) == 3 && expr[[1]] == '<-'

			}, logical(1)) )

		# -- you must start with a yield expression.
		if (any(binding_indices == 1)) {

			message <-
				"a collection-comprehension must not begin with a " %+%
				"variable bind expression."

			throw_kea_error(invoking_call, message)
		}

		# -- you can't name the expressions!
		if ( !is.null(names(exprs)) ) {

			message <-
				"a collection-comprehension cannot have named sub-terms."

			throw_kea_error(invoking_call, message)
		}

		bindings <- exprs[binding_indices]

		# -- the first expression is always the yield expression.
		self$yield <-
			exprs[[1]]

		# -- is the last expression a non-binding expression?
		is_predicated <-
			length(exprs) %!in% binding_indices && length(exprs) > 1

		# -- the selection predicate defaults to true.
		self$predicate <-
			if (is_predicated) {
				exprs[[ length(exprs) ]]
			} else {
				True
			}

		# -- are there any expressions that aren't the yield expression, binding
		# -- expression or predicate?
		unmatched <- if (is_predicated) {
			expr_indices <- seq_along(exprs)
			expr_indices[ expr_indices %!in% c(1, binding_indices, length(exprs)) ]
		} else {
			expr_indices <- seq_along(exprs)
			expr_indices[expr_indices %!in% c(1, binding_indices)]
		}

		# -- some abnormal expressions
		if (length(unmatched) != 0) {

			unmatched_str <- paste0(lapply(unmatched, ith_suffix), collapse = ', ')

			message <-
				"the " %+% unmatched_str %+% " expression " %+%
				"could not be matched as variable bindings, a predicate, or " %+%
				"a yield expression."

			throw_kea_error(invoking_call, message)
		}

		# -- check that all expressions are matched.

		self$variables <-
			vapply(bindings, function (expr) {
				paste0( expr[[2]] )
			}, character(1))

		# -- some variable was matched multiple times (x <- 1:10, x <- letters)
		if ( any(duplicated(self$variables)) ) {

			duplicated_var <- self$variables[duplicated(self$variables)]

			message <-
				"The variables " %+% paste0(duplicated_var, collapse = ', ') %+%
				" were bound several times by binding expressions."

				throw_kea_error(invoking_call, message)
		}

		# -- no variables were bound (x <- 1:10)
		if (length(self$variables) == 0) {

			message <-
				"a non-empty collection-comprehension must have " %+%
				"at least one variable binding."

			throw_kea_error(invoking_call, message)
		}

		# -- evaluate coll in the expression x <- coll
		self$values <-
			lapply(bindings, function (expr) {
				eval(expr[[3]], envir = parent_frame)
			})

		self
	})

	parametreised <- local({

		self <- list()

		parametreise <- function (expr, params) {

			# -- add parametres to the expression, creating a function.
			fn <- do.call('function', list(
				as.pairlist(as_formals(params)),
				expr
			))
			# -- evaluate in the parent environment.
			environment(fn) <- parent_frame
			fn
		}

		self$yield <- parametreise(
			components$yield, components$variables)

		self$predicate <- parametreise(
			components$predicate, components$variables)

		self
	})

	xMap(
		xUnspread(parametreised $ yield),
		xSelect(
			xUnspread(parametreised$predicate),
			xProdSetOf(components$values)
		)
	)
}
