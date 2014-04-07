
#' x.
#'
#' Wildcard
#'
#'
#'
#'
#'
#'

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

'%%.xlambda_builder' <- function (x, y) {
	function (val) {
		"a function created by x. %%"
		val %% y
	}
}

#' @export
#' @rdname x.

'%/%.xlambda_builder' <- function (x, y) {
	function (val) {
		"a function created by x. %/%"
		val %/% y
	}
}

#' @export
#' @rdname x.

'::.xlambda_builder' <- function (x, y) {
	function (val) {
		"a function created by x. ::"
		val :: y
	}
}

#' @export
#' @rdname x.

'$.xlambda_builder' <- function (x, y) {
	function (val) {
		"a function created by x. $"
		val $ y
	}
}

#' @export
#' @rdname x.

'@.xlambda_builder' <- function (x, y) {
	function (val) {
		"a function created by x. @"
		val @ y
	}
}

#' @export
#' @rdname x.

'^.xlambda_builder' <- '**.xlambda_builder' <- function (x, y) {
	function (val) {
		"a function created by x. ^"
		val ^ y
	}
}

#' @export
#' @rdname x.

':.xlambda_builder' <- function (x, y) {
	function (val) {
		"a function created by x. :"
		val : y
	}
}

#' @export
#' @rdname x.

'*.xlambda_builder' <- function (x, y) {
	function (val) {
		"a function created by x. *"
		val * y
	}
}

#' @export
#' @rdname x.

'/.xlambda_builder' <- function (x, y) {
	function (val) {
		"a function created by x. /"
		val / y
	}
}

#' @export
#' @rdname x.

'-.xlambda_builder' <- function (x, y) {
	function (val) {
		"a function created by x. -"
		val - y
	}
}

#' @export
#' @rdname x.

'+.xlambda_builder' <- function (x, y) {
	function (val) {
		"a function created by x. +"
		val + y
	}
}

#' @export
#' @rdname x.

'>.xlambda_builder' <- function (x, y) {
	function (val) {
		"a function created by x. >"
		val > y
	}
}

#' @export
#' @rdname x.

'>=.xlambda_builder' <- function (x, y) {
	function (val) {
		"a function created by x. >="
		val >= y
	}
}

#' @export
#' @rdname x.

'<.xlambda_builder' <- function (x, y) {
	function (val) {
		"a function created by x. <"
		val < y
	}
}

#' @export
#' @rdname x.

'<=.xlambda_builder' <- function (x, y) {
	function (val) {
		"a function created by x. <="
		val <= y
	}
}

#' @export
#' @rdname x.

'==.xlambda_builder' <- function (x, y) {
	function (val) {
		"a function created by x. =="
		val == y
	}
}

#' @export
#' @rdname x.

'!=.xlambda_builder' <- function (x, y) {
	function (val) {
		"a function created by x. !="
		val != y
	}
}

#' @export
#' @rdname x.

'&.xlambda_builder' <- function (x, y) {
	function (val) {
		"a function created by x. &"
		val & y
	}
}

#' @export
#' @rdname x.

'&&.xlambda_builder' <- function (x, y) {
	function (val) {
		"a function created by x. &&"
		val && y
	}
}

#' @export
#' @rdname x.

'|.xlambda_builder' <- function (x, y) {
	function (val) {
		"a function created by x. |"
		val | y
	}
}

#' @export
#' @rdname x.

'||.xlambda_builder' <- function (x, y) {
	function (val) {
		"a function created by x. ||"
		val || y
	}
}

#' @export
#' @rdname x.

'~.xlambda_builder' <- function (x, y) {
	function (val) {
		"a function created by x. ~"
		val ~ y
	}
}
