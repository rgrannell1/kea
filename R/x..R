
#' x.
#'
#' Partially apply an infix function.
#'
#' @details
#'     One of the most common uses of anonymous functions is
#'     to partially apply a binary function.
#'
#' @param
#'     dropped the value x.. This isn't used internally.
#'
#' @param
#'     ..2 the right hand side of an infix function call.
#'
#' @return an arbitrary value.
#'
#' @example
#'    inst/examples/example-x..R
#'
#' @rdname x.
#' @export

x. <- structure(
	function (...) {

		invoking_call <- sys.call()

		message <-
			"lambda builder objects cannot be invoked as a " %+%
			"function."

		throw_arrow_error(invoking_call, message)
	},
	class = 'xlambda_builder'
)

#' @export
#' @rdname x.

'%%.xlambda_builder' <- function (...) {
    function (val) {
        "a function created by x. %%"
        val %% ..2
    }
}

#' @export
#' @rdname x.

'%/%.xlambda_builder' <- function (...) {
	function (val) {
		"a function created by x. %/%"
		val %/% ..2
	}
}

#' @export
#' @rdname x.

'[[.xlambda_builder' <- function (...) {
	function (val) {
		"a function created by x. [["
		val[[..2]]
	}
}

#' @export
#' @rdname x.

'[.xlambda_builder' <- function (...) {
	function (val) {
		"a function created by x. ["
		val[..2]
	}
}

#' @export
#' @rdname x.

'$.xlambda_builder' <- function (...) {

	..2 <- match.call()$..2

	function (val) {
		"a function created by x. $"

		do.call('$', list(val, ..2))
	}
}

#' @export
#' @rdname x.

'@.xlambda_builder' <- function (...) {

	..2 <- match.call()$..2

	function (val) {
		"a function created by x. @"

		do.call('@', list(val, ..2))
	}
}

#' @export
#' @rdname x.

'^.xlambda_builder' <- '**.xlambda_builder' <- function (...) {
	function (val) {
		"a function created by x. ^"
		val ^ ..2
	}
}

#' @export
#' @rdname x.

':.xlambda_builder' <- function (...) {
	function (val) {
		"a function created by x. :"
		val : ..2
	}
}

#' @export
#' @rdname x.

'*.xlambda_builder' <- function (...) {
	function (val) {
		"a function created by x. *"
		val * ..2
	}
}

#' @export
#' @rdname x.

'/.xlambda_builder' <- function (...) {
	function (val) {
		"a function created by x. /"
		val / ..2
	}
}

#' @export
#' @rdname x.

'-.xlambda_builder' <- function (...) {

	if (missing(..2)) {
		"a function created by -x."
		function (val) -val
	} else {
		function (val) {
			"a function created by x. -"
			val - ..2
		}
	}
}

#' @export
#' @rdname x.

'+.xlambda_builder' <- function (...) {

	if (missing(..2)) {
		"a function created by +x."
		function (val) +val
	} else {
		function (val) {
			"a function created by x. +"
			val + ..2
		}
	}
}

#' @export
#' @rdname x.

'>.xlambda_builder' <- function (...) {
	function (val) {
		"a function created by x. >"
		val > ..2
	}
}

#' @export
#' @rdname x.

'>=.xlambda_builder' <- function (...) {
	function (val) {
		"a function created by x. >="
		val >= ..2
	}
}

#' @export
#' @rdname x.

'<.xlambda_builder' <- function (...) {
	function (val) {
		"a function created by x. <"
		val < ..2
	}
}

#' @export
#' @rdname x.

'<=.xlambda_builder' <- function (...) {
	function (val) {
		"a function created by x. <="
		val <= ..2
	}
}

#' @export
#' @rdname x.

'==.xlambda_builder' <- function (...) {
	function (val) {
		"a function created by x. =="
		val == ..2
	}
}

#' @export
#' @rdname x.

'!=.xlambda_builder' <- function (...) {
	function (val) {
		"a function created by x. !="
		val != ..2
	}
}

#' @export
#' @rdname x.

'&.xlambda_builder' <- function (...) {
	function (val) {
		"a function created by x. &"
		val & ..2
	}
}

#' @export
#' @rdname x.

'&&.xlambda_builder' <- function (...) {
	function (val) {
		"a function created by x. &&"
		val && ..2
	}
}

#' @export
#' @rdname x.

'|.xlambda_builder' <- function (...) {
	function (val) {
		"a function created by x. |"
		val | ..2
	}
}

#' @export
#' @rdname x.

'||.xlambda_builder' <- function (...) {
	function (val) {
		"a function created by x. ||"
		val || ..2
	}
}

#' @export
#' @rdname x.

'!.xlambda_builder' <- function (dropped) {
	function (val) {
		"a function created by !x."
		!val
	}
}
