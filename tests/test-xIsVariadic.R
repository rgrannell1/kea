
forall <- kiwi:::forall
test_cases <- kiwi:::test_cases

require(kiwi)

message("xIsVariadic (+)")

	over(fn) +

	describe('logical(0) for nullary functions') +
	when(
		is.function(fn) && length(formals(fn)) == 0,
		xIsVariadic(fn) %equals% logical(0)
	) +

	describe("when xIsVariadic, ... is in the function params.") +
	when(
		is.function(fn) && isTRUE(xIsVariadic(fn)),
		'...' %in% names(formals(fn))
	) +

	describe("when ... is in the function params, xIsVariadic") +
	when(
		is.function(fn) && isTRUE( '...' %in% names(formals(fn)) ),
		xIsVariadic(fn)
	) +

	describe("only true when ... in params") +
	when(
		is.function(fn) && !isTRUE( '...' %in% names(formals(fn)) ),
		!xIsVariadic(fn)
	) +

	run()
