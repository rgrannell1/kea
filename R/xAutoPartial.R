
xAutoPartial <- function (fn) {
	# function -> function
	# partially apply a function

	this <- list(
		stored_args = 
			list(),
		underlying = 
			xAsClosure(match.fun(fn))
	)

	if ('...' %in% xParametres(fn)) {
		return (fn)
	}
	do.call('function', list(
		as.pairlist(xFormals(fn)), quote({
			# --- an accumulator function, wrapping 
			# an underlying function.

			this$def <- sys.function()
			this$pcall <- match.call(this$def, sys.call())
			this$pframe <- parent.frame()

			this$args <- Map(
				function (name) {
					# --- don't evaluate argument.
					this$pcall[[name]]
				},
				names(this$pcall))

			if (length(this$args) == 0) {
				return (this$def)
			}

			maybe_invoke(
				this,
				accum = fix_args(this))
	}) ))
}

fix_args <- function (this) {
	# defined seperately to dodge lexical scoping.

	accum <- this$def
	
	environment(accum) <- ( function() {
		# --- copy this$def, fix more arguments.

		tmp <- new.env(parent = emptyenv())

		tmp$this <- list(
			fixed =
				c(this$fixed, args),
			underlying = 
				this$underlying
		)
		tmp
	} )()

	formals(accum) <- ( function () {
		# --- Act One: construct a new accumulator.

		params <- names(formals(this$underlying))
		free <- setdiff(params, names(this$fixed))
		xFormals(this$underlying)[free]
	} )()

	accum
}

maybe_invoke <- function (this, accum) {
	# --- Act Two: If the accumulator is full,
	# --- invoke underlying function. Otherwise return.

	if (xArity(accum) == 0) {
		do.call(
			environment(accum)$this$underlying,
			environment(accum)$this$fixed
		)
	} else {
		accum
	}
}

xAutoPartial( function (a, b, c) a + b + c )
