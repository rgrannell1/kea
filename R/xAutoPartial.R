
xAutoPartial <- function (fn) {
	# function -> function
	# takes a n argument function to a function
	# that can take less than n arguments at a time, 
	# eventually passing them to fn.

	if ( '...' %in% names(formals(fn)) ) {
		return (fn)
	} else {
		acc_apply(this = list(
			fixed = 
				list(),
			fn = 
				xAsClosure(match.fun(fn)) ))
	}
}

acc_apply <- function (this) {
	# list -> function | any

	do.call('function', list(
		as.pairlist(xFormals(this$fn)), quote({
			# --- an accumulator function, wrapping 
			# an underlying function.

			# --- Act One: capture function and call information.
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
				#  call the underlying function. Or return accumulate.

				if (xArity(acc) == 0) {
					do.call(this$fn, this$fixed)
				} else {
					acc
				}
			}
		})
	))
}

fix_args <- function (this) {
	# list -> function
	# return an accumulator with some more arguments
	# fixed

	acc <- this$def
	environment(acc) <- ( function() {
		# --- fix more arguments.

		tmp <- new.env(parent = this$pframe)
		tmp$this <- list(
			fixed =
				c(this$fixed, this$args),
			fn = 
				this$fn
		)
		tmp
	} )()
	
	this <- environment(acc)$this

	formals(acc) <- 
		xFormals(this$fn)[
			setdiff(
				names(formals(this$fn)),
				names(this$fixed)) 
		]
	acc
}
