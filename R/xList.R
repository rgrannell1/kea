
#' xList[ ]
#'
#' Generate a list using set-builder notation.
#'
#' @details
#'    \bold{xList} allows for shorthand construction
#'    of lists using a form of set-builder notation.
#'    The notation typically looks like
#'
#'    \code{S = {2.x | x ε 1..100, x^2 > 3}}
#'
#'    which reads "S is the set of numbers '2x' from the
#'    numbers 1...100, where x^2 is larger than three".
#'    In general the syntax is of the form
#'
#'    \code{S = {output expression, variable ε set, predicate expression}}
#'
#'    Arrow's syntax is similar
#'
#'    \code{S <- xList[ 2*x, x <- 1:10 x^2 > 3 ]}
#'
#'    \code{S <- xList[output expression, variable <- set, predicate expression]}
#'
#'    Collection comprehensions are a shorthand for several operations; taking
#'    the \bold{set product} of several Collections, \bold{selecting} them based
#'    on a predicate, and \bold{mapping} a function over each result.
#'
#'    \bold{1 The output expression}
#'
#'    The output expression corresponds to the map operation mentioned above.
#'    The for comprehension
#'
#'    \code{xList[2*x, x <- 1:10]}
#'
#'    is loosely translated into normal arrow code as
#'
#'    \code{xMapply(x := x^2, 1:10)}
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
#'    \code{x_(xSetProd(1:3, 1:3)) $ xMapply((x : y) := c(x, y))}
#'
#'    \bold{3 The predicate expression (optional)}
#'
#'    If the final expression given to a comprehension is not a binding
#'    expression it is treated as a predicate for selecting particular
#'    values. This is optional.
#'
#'    \code{xList[c(a, b), a <- 1:3, b <- 1:3, a + b > 2]}
#'
#'    In the above pairs of a, b such that their sum is larger than
#'    two are returned.
#' @rdname xList
#' @export

xList <- structure(
	function (...) {

		invoking_call <- sys.call()

		message <-
			"comprehension objects cannot be invoked as a " %+%
			"function: they must be invoked with square brackets ( [] )"

		throw_arrow_error(invoking_call, message)
	},
	class = 'list_builder'
)

#' @method print list_builder
#' @export

print.list_builder <- function (x, ...) {
	cat("[ the xList object ]\n")
}

#' @method [ list_builder
#' @export

'[.list_builder' <- function (x, ...) {

	exprs <- eval( substitute(alist(...)) )
	parent_frame <- parent.frame()

	invoking_call <- match.call()[-1]

	if ( identical(exprs[[1]], quote(expr=)) ) {
		return(list())
	}

	components <- local({

		this <- list()
		binding_indices <-
			which( vapply(exprs, function (expr) {

				length(expr) == 3 && expr[[1]] == '<-'

			}, logical(1)) )

		if (1 %in% binding_indices) {

			message <-
				"a collection-comprehension must not begin with a " %+%
				"variable bind expression."

			throw_arrow_error(invoking_call, message)
		}

		if ( !is.null(names(exprs)) ) {

			message <-
				"a collection-comprehension cannot have named sub-terms."

			throw_arrow_error(invoking_call, message)
		}

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

		unmatched <- if (is_predicated) {
			expr_indices <- seq_along(exprs)
			expr_indices[ expr_indices %!in% c(1, binding_indices, length(exprs)) ]
		} else {
			expr_indices <- seq_along(exprs)
			expr_indices[expr_indices %!in% c(1, binding_indices)]
		}

		if (length(unmatched) != 0) {

			unmatched_str <- paste0(lapply(unmatched, ith_suffix), collapse = ', ')

			message <-
				"the " %+% unmatched_str %+% " expression " %+%
				"could not be matched as variable bindings, a predicate, or " %+%
				"a yield expression."

			throw_arrow_error(invoking_call, message)
		}

		# check that all expressions are matched.

		this$variables <-
			vapply(bindings, function (expr) {
				paste0( expr[[2]] )
			}, character(1))

		if ( any(duplicated(this$variables)) ) {

			duplicated_var <- this$variables[duplicated(this$variables)]

			message <-
				"The variables " %+% paste0(duplicated_var, collapse = ', ') %+%
				" were bound several times by binding expressions."

				throw_arrow_error(invoking_call, message)
		}


		if (length(this$variables) == 0) {

			message <-
				"a non-empty collection-comprehension must have " %+%
				"at least one variable binding."

			throw_arrow_error(invoking_call, message)
		}

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

	xMapply(
		parametreised$yield,
		xSelect(
			xAsUnary(parametreised$predicate),
			xProdSetOf(components$values)
		)
	)
}
