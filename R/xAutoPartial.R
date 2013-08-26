
xAutoPartial <- function (fn) {
	# function -> function
	# 

	this <- list(
		stored_args = 
			list(),
		underlying = 
			match.fun(fn)
	)

	if ('...' %in% xParametres(fn)) {
		return(fn)
	}
	do.call('function', list(
		as.pairlist(xFormals(fn)), quote({
			# --- an accumulator function, wrapping 
			# an underlying function.

			this$def <- sys.function()
			this$pcall <- sys.call()
			this$pframe <- parent.frame()

			args <- Map(
				function (name) {
					eval(as.symbol(name), this$pframe)
				},
				names(this$pcall)
			)

			if (length(args) == 0) {
				return (this$def)
			}

			# --- Act One: construct a new accumulator

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
				# --- set the formals to the unfixed parameters.
				params <- xParametres(this$underlying)
				free <- !(params %in% names(this$fixed))
				xFormals(this$underlying)[free]

			} )()

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
	})) )
}

xAutoPartial( function (a, b, c) a + b + c )
