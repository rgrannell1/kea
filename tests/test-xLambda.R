
# note - shorten imports

':=' <- arrow:::':='
atoms <- arrow:::atoms
forall <- arrow:::forall
compounds <- arrow:::compounds
test_cases <- arrow:::test_cases

require(arrow)


arrow_unary <- function () {

	arrow:::one_of(
		list(
			x := {
				x
			},
			(x) := {
				x
			},
			x := x,
			(x) := x
	))
}

arrow_binary <- function () {

	arrow:::one_of(
		list(
			(a : b) := {
				a + b
			},
			(a : b) := a + b
	))
}

arrow_trinary <- function () {

	arrow:::one_of(
		list(
			(a : b : c) := {
				a + b + c
			},
			(a : b : c) := a + b + c
	))
}

arrow_unary_variadic <- function () {

	arrow:::one_of(
		list(
			(...) := {
				list(...)
			},
			(...) := {
				list(...)
			}
	))
}

arrow_binary_variadic <- function () {

	arrow:::one_of(
		list(
			(a : ...) := {
				sum(a, ...)
			},
			(a : ...) := {
				sum(a, ...)
			}
	))
}

message(":=")

message("non_variadic functions")

	forall(
		"all forms of unary functions are functioning.",
		list(id = arrow_unary, coll = compounds$collection),
		id(coll) %equals% coll
	)

	forall(
		"all forms of binary functions are functioning.",
		list(plus2 = arrow_binary, a = atoms$integer(), b = atoms$integer()),
		plus2(a, b) == a + b
	)

	forall(
		"all forms of trinary functions are functioning.",
		list(
			plus3 = arrow_trinary,
			a = atoms$integer(),
			b = atoms$integer(),
			c = atoms$integer()),
		plus3(a, b, c) == a + b + c
	)

message("variadic functions")

	forall(
		"all forms of variadic unary functions are functioning.",
		list(
			id = arrow_unary_variadic,
			coll = compounds$collection),
		id(coll) %equals% list(coll)
	)

	forall(
		"all forms of variadic binary functions are functioning.",
		list(
			plus2 = arrow_binary_variadic,
			a = atoms$integer(),
			b = atoms$integer()),
		plus2(a, b) == a + b
	)
