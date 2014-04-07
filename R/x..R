
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
#'     val2 the right hand side of an infix function call.
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

'%%.xlambda_builder' <- function (dropped, val2) {
    function (val1) {
        "a function created by x. %%"
        val1 %% val2
    }
}

#' @export
#' @rdname x.

'%/%.xlambda_builder' <- function (dropped, val2) {
	function (val1) {
		"a function created by x. %/%"
		val1 %/% val2
	}
}

#' @export
#' @rdname x.

'[[.xlambda_builder' <- function (dropped, val2) {
	function (val1) {
		"a function created by x. [["
		val1[[val2]]
	}
}

#' @export
#' @rdname x.

'[.xlambda_builder' <- function (dropped, val2) {
	function (val1) {
		"a function created by x. ["
		val1[val2]
	}
}

#' @export
#' @rdname x.

'$.xlambda_builder' <- function (dropped, val2) {

	val2 <- match.call()$val2

	function (val1) {
		"a function created by x. $"

		do.call('$', list(val1, val2))
	}
}

#' @export
#' @rdname x.

'@.xlambda_builder' <- function (dropped, val2) {

	val2 <- match.call()$val2

	function (val1) {
		"a function created by x. @"

		do.call('@', list(val1, val2))
	}
}

#' @export
#' @rdname x.

'^.xlambda_builder' <- '**.xlambda_builder' <- function (dropped, val2) {
	function (val1) {
		"a function created by x. ^"
		val1 ^ val2
	}
}

#' @export
#' @rdname x.

':.xlambda_builder' <- function (dropped, val2) {
	function (val1) {
		"a function created by x. :"
		val1 : val2
	}
}

#' @export
#' @rdname x.

'*.xlambda_builder' <- function (dropped, val2) {
	function (val1) {
		"a function created by x. *"
		val1 * val2
	}
}

#' @export
#' @rdname x.

'/.xlambda_builder' <- function (dropped, val2) {
	function (val1) {
		"a function created by x. /"
		val1 / val2
	}
}

#' @export
#' @rdname x.

'-.xlambda_builder' <- function (dropped, val2) {
	function (val1) {
		"a function created by x. -"
		val1 - val2
	}
}

#' @export
#' @rdname x.

'+.xlambda_builder' <- function (dropped, val2) {
	function (val1) {
		"a function created by x. +"
		val1 + val2
	}
}

#' @export
#' @rdname x.

'>.xlambda_builder' <- function (dropped, val2) {
	function (val1) {
		"a function created by x. >"
		val1 > val2
	}
}

#' @export
#' @rdname x.

'>=.xlambda_builder' <- function (dropped, val2) {
	function (val1) {
		"a function created by x. >="
		val1 >= val2
	}
}

#' @export
#' @rdname x.

'<.xlambda_builder' <- function (dropped, val2) {
	function (val1) {
		"a function created by x. <"
		val1 < val2
	}
}

#' @export
#' @rdname x.

'<=.xlambda_builder' <- function (dropped, val2) {
	function (val1) {
		"a function created by x. <="
		val1 <= val2
	}
}

#' @export
#' @rdname x.

'==.xlambda_builder' <- function (dropped, val2) {
	function (val1) {
		"a function created by x. =="
		val1 == val2
	}
}

#' @export
#' @rdname x.

'!=.xlambda_builder' <- function (dropped, val2) {
	function (val1) {
		"a function created by x. !="
		val1 != val2
	}
}

#' @export
#' @rdname x.

'&.xlambda_builder' <- function (dropped, val2) {
	function (val1) {
		"a function created by x. &"
		val1 & val2
	}
}

#' @export
#' @rdname x.

'&&.xlambda_builder' <- function (dropped, val2) {
	function (val1) {
		"a function created by x. &&"
		val1 && val2
	}
}

#' @export
#' @rdname x.

'|.xlambda_builder' <- function (dropped, val2) {
	function (val1) {
		"a function created by x. |"
		val1 | val2
	}
}

#' @export
#' @rdname x.

'||.xlambda_builder' <- function (dropped, val2) {
	function (val1) {
		"a function created by x. ||"
		val1 || val2
	}
}
