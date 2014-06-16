
kiwi ::: load_test_dependencies(environment())
is_collection <- kiwi ::: is_collection

require(kiwi)

message("xIsVariadic (+)")

	over(fn) +

	describe('logical(0) for nullary functions') +
	when(
		is.function(fn) && length(xFormalsOf(fn)) == 0,
		xIsVariadic(fn) %is% logical(0)
	) +

	describe("when xIsVariadic, ... is in the function params.") +
	when(
		is.function(fn) && isTRUE(xIsVariadic(fn)),
		'...' %in% names(xFormalsOf(fn))
	) +

	describe("when ... is in the function params, xIsVariadic") +
	when(
		is.function(fn) && isTRUE( '...' %in% names(xFormalsOf(fn)) ),
		xIsVariadic(fn)
	) +

	describe("only true when ... in params") +
	when(
		is.function(fn) && !isTRUE( '...' %in% names(xFormalsOf(fn)) ),
		!isTRUE(xIsVariadic(fn))
	) +

	run()
