
xAutoPartial <- function (fn) {
	# function -> function
	# takes a n argument function to a function
	# that can take less than n arguments at a time, 
	# eventually passing them to fn.

	if ( '...' %in% names(formals(fn)) ) {
		return (fn)
	} else {
		acc_apply(this = list(
			stored_args = 
				list(),
			underlying = 
				xAsClosure(match.fun(fn)) ))
	}
}

acc_apply <- function (this) {

	do.call('function', list(
		as.pairlist(xFormals(this$underlying)), quote({
			# --- an accumulator function, wrapping 
			# an underlying function.

			# --- Act One: capture function information.
			this$def <- 
				sys.function()
			this$pcall <- 
				match.call(this$def, sys.call())[-1]
			this$pframe <- 
				parent.frame()
			this$args <- 
				lapply(
					names(this$pcall),
					function (name) {
						this$pcall[[ name ]] #lazy
					})
			names(this$args) <-
				names(this$pcall)

			if (length(this$args) == 0) {
				this$def
			} else {
				# --- Act Two: construct a new accumulator,
				# with more arguments fixed.
				acc <- fix_args(this)
				
				# --- Act Three: If the accumulator is full,
				#  invoke the underlying function. Or return accumulate.

				maybe_invoke(acc, this)	
			}
		})
	))
}

fix_args <- function (this) {
	# list -> function

	acc <- this$def
	environment(acc) <- ( function() {
		# --- fix more arguments.

		tmp <- new.env(parent = emptyenv())
		tmp$this <- list(
			fixed =
				c(this$fixed, this$args),
			underlying = 
				this$underlying
		)
		tmp
	} )()

	formals(acc) <- ( function () {
		# --- set the formals to the unbound function.

		this <- environment(acc)$this
		params <- names(formals(this$underlying))
		free <- setdiff(params, names(this$fixed))
		xFormals(this$underlying)[free]

	} )()

	acc
}

maybe_invoke <- function (acc, this) {
	# function -> list -> function | any
	# either invoke the underlying function,
	# or return the updated accumulator.

	if (xArity(acc) == 0) {
		do.call(
			what = environment(acc)$this$underlying,
			args = environment(acc)$this$fixed)
	} else {
		acc
	}
}

#xAutoPartial( function (a, b, c) a + b + c )
