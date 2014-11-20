
# Sample is insane in R.
# sample(10, size = 1) ~ 10, which makes it awful for
# shuffling random integer vectors.

rsample <- function (coll, ...) {

	if (is.numeric(coll) && length(coll) == 1) {
		coll
	} else {
		sample(coll, ...)
	}
}







is_na <- function (val) {
	# -- is.na fails for Null, NaN and other annoying cases.
	# -- na can be named.
	is_atomic(val) && length(val) == 1 && unname(is.na(val) && !is.nan(val))
}

is_nan <- function (val) {
	# -- NaN can be named.
	is_atomic(val) && length(val) == 1 && is.nan(val)
}





elem_is_na <- function (coll) {

	if (is_atomic(coll))
		is.na(coll) & !is.nan(coll)
	else
		vapply(coll, function (elem) {
			is_atomic(elem) && length(elem) == 1 && unname(is.na(elem) && !is.nan(elem))
		}, logical(1), USE.NAMES = True)

}

elem_is_nan <- function (coll) {

	if (is_atomic(coll))
		is.nan(coll)
	else
		vapply(coll, function (elem) {
			is_atomic(elem) && length(elem) == 1 && is.nan(elem)
		}, logical(1), USE.NAMES = True)

}










# -- corrects the null corner case of is.atomic

is_atomic <- function (coll) {
	is.atomic(coll) && !inherits(coll, 'factor') && !is.null(coll)
}

# -- corrects the null corner case of is.list

is_generic <- function (coll) {
	is.list(coll) || is.null(coll)
}










# -- checks identity, doesn't do odd things for nan.

'%is_in%' <- function (coll1, coll2) {

	if (length(coll1) == 0) {
		logical(0)
	} else if (length(coll2) == 0) {
		# -- the base function does this; should the replacement?
		False
	} else {

		vapply(coll1, function (elem1) {

			any( vapply(coll2, function (elem2) {
				identical(elem1, elem2)
			}, logical(1)) )

		}, logical(1), USE.NAMES = False)

	}
}

# -- more useful than is.recursive

is_recursive <- function (val) {
	# -- don't change. is.recursive is ~ !is.atomic.
	# -- is list checks lists, pairlists. Add null check.
	is.list(val) || identical(val, NULL)
}

# -- strsplit has a bad case of overcomplicated type signature,
# -- and it adds leading spaces.

str_split <- function (rexp, str) {
	if (length(str) == 0 || nchar(str) == 0) {
		character(0)
	} else {
		out <- strsplit(str, rexp)[[1]]

		if (out[1] == '') {
			out[2:length(out)]
		} else {
			out
		}
	}
}

is_named <- function (coll) {
	!is.null(names(coll))
}

as_named <- function (coll) {
	if (length(coll) == 0) {
		structure(coll, names = character(0))
	} else {
		stop('as_named')
	}
}

keep_names <- function (coll1, coll2) {

	if ( length(coll1) == 0 && !is.null(names(coll2)) ) {

		names(coll1) <- character(0)
		coll1

	} else {
		coll1
	}
}
