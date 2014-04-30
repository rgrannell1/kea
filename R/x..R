
#' x.
#'
#' Partially apply an infix function.
#'
#' @details
#'     One of the most common uses of anonymous functions is
#'     to partially apply a binary function. In the toy example
#'     below an anonymous function is used to check if a string is empty.
#'
#'     \code{str <-  c(' ', 'hello', ' ','cruel', ' ', 'world')}
#'
#'     \code{xReject(function (x) x == ' ', str)}
#'
#'     Arrow has a general short-hand syntax for creating anonymous functions
#'     already, which works for arbitrary functions.
#'
#'     \code{xReject(x := x == ' ', str)}
#'
#'     but for the simplest anonymous functions a second syntax exists.
#'
#'     \code{xReject(x. == ' ', str)}
#'
#'     The above functions are all identical, but the final example is
#'     the most consise. This syntax is available to most of R's builtin
#'     infix operators (+, **, etc.), its indexing operators ([[, $, etc.)
#'     and its unary operators (!, +, -).
#'
#' @param
#'     ... the argument passed to x.. The right hand side of the operator.
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

#' @usage x. %% ...
#'
#' @method %% xlambda_builder
#' @export
#' @rdname x.

`%%.xlambda_builder` <- function (...) {
    function (val) {
        "a function created by x. %%"
        val %% ..2
    }
}

#' @usage x. %/% ...
#'
#' @method %/% xlambda_builder
#' @export
#' @rdname x.

`%/%.xlambda_builder` <- function (...) {
	function (val) {
		"a function created by x. %/%"
		val %/% ..2
	}
}


#' @usage x. [[...]]
#'
#' @method [[ xlambda_builder
#' @export
#' @rdname x.

`[[.xlambda_builder` <- function (...) {
	function (val) {
		"a function created by x. [["
		val[[..2]]
	}
}

#' @usage x. [...]
#'
#' @method [ xlambda_builder
#' @export
#' @rdname x.

`[.xlambda_builder` <- function (...) {
	function (val) {
		"a function created by x. ["
		val[..2]
	}
}

#' @usage x. $ ...
#'
#' @method $ xlambda_builder
#' @export
#' @rdname x.

`$.xlambda_builder` <- function (...) {

	..2 <- match.call()$..2

	function (val) {
		"a function created by x. $"

		do.call('$', list(val, ..2))
	}
}

#' @usage x. @@ ...
#'
#' @method @@ xlambda_builder
#' @export
#' @rdname x.

`@.xlambda_builder` <- function (...) {

	..2 <- match.call()$..2

	function (val) {
		"a function created by x. @"

		do.call('@', list(val, ..2))
	}
}

#' @usage x. ^ ...
#'
#' @method ^ xlambda_builder
#' @export
#' @rdname x.

`^.xlambda_builder` <- '**.xlambda_builder' <- function (...) {
	function (val) {
		"a function created by x. ^"
		val ^ ..2
	}
}

#' @usage x. : ...
#'
#' @method : xlambda_builder
#' @export
#' @rdname x.

`:.xlambda_builder` <- function (...) {
	function (val) {
		"a function created by x. :"
		val : ..2
	}
}

#' @usage x. * ...
#'
#' @method * xlambda_builder
#' @export
#' @rdname x.

`*.xlambda_builder` <- function (...) {
	function (val) {
		"a function created by x. *"
		val * ..2
	}
}

#' @usage x. / ...
#'
#' @method / xlambda_builder
#' @export
#' @rdname x.

`/.xlambda_builder` <- function (...) {
	function (val) {
		"a function created by x. /"
		val / ..2
	}
}

#' @usage x. - ...
#' @usage -x.
#'
#' @method - xlambda_builder
#' @export
#' @rdname x.

`-.xlambda_builder` <- function (...) {

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

#' @usage x. + ...
#' @usage +x.
#'
#' @method + xlambda_builder
#' @export
#' @rdname x.

`+.xlambda_builder` <- function (...) {

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

#' @usage x. > ...
#'
#' @method > xlambda_builder
#' @export
#' @rdname x.

`>.xlambda_builder` <- function (...) {
	function (val) {
		"a function created by x. >"
		val > ..2
	}
}

#' @usage x. >= ...
#'
#' @method >= xlambda_builder
#' @export
#' @rdname x.

`>=.xlambda_builder` <- function (...) {
	function (val) {
		"a function created by x. >="
		val >= ..2
	}
}

#' @usage x. < ...
#'
#' @method < xlambda_builder
#' @export
#' @rdname x.

`<.xlambda_builder` <- function (...) {
	function (val) {
		"a function created by x. <"
		val < ..2
	}
}

#' @usage x. <= ...
#'
#' @method <= xlambda_builder
#' @export
#' @rdname x.

`<=.xlambda_builder` <- function (...) {
	function (val) {
		"a function created by x. <="
		val <= ..2
	}
}

#' @usage x. == ...
#'
#' @method == xlambda_builder
#' @export
#' @rdname x.

`==.xlambda_builder` <- function (...) {
	function (val) {
		"a function created by x. =="
		val == ..2
	}
}

#' @usage x. != ...
#'
#' @method != xlambda_builder
#' @export
#' @rdname x.

`!=.xlambda_builder` <- function (...) {
	function (val) {
		"a function created by x. !="
		val != ..2
	}
}

#' @usage x. & ...
#'
#' @method & xlambda_builder
#' @export
#' @rdname x.

`&.xlambda_builder` <- function (...) {
	function (val) {
		"a function created by x. &"
		val & ..2
	}
}

#' @usage x. && ...
#'
#' @method && xlambda_builder
#' @export
#' @rdname x.

`&&.xlambda_builder` <- function (...) {
	function (val) {
		"a function created by x. &&"
		val && ..2
	}
}

#' @usage x. | ...
#'
#' @method | xlambda_builder
#' @export
#' @rdname x.

`|.xlambda_builder` <- function (...) {
	function (val) {
		"a function created by x. |"
		val | ..2
	}
}

#' @usage x. || ...
#'
#' @method || xlambda_builder
#' @export
#' @rdname x.

`||.xlambda_builder` <- function (...) {
	function (val) {
		"a function created by x. ||"
		val || ..2
	}
}

#' @usage !x.
#'
#' @method ! xlambda_builder
#' @export
#' @rdname x.

`!.xlambda_builder` <- function (...) {
	function (val) {
		"a function created by !x."
		!val
	}
}
