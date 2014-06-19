
# note - shorten imports

':=' <- kiwi:::':='
atoms <- kiwi:::atoms
forall <- kiwi:::forall
compounds <- kiwi:::compounds
test_cases <- kiwi:::test_cases

require(kiwi)


kiwi_unary <- function () {

	kiwi:::one_of(
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

kiwi_binary <- function () {

	kiwi:::one_of(
		list(
			(a : b) := {
				a + b
			},
			(a : b) := a + b
	))
}

kiwi_trinary <- function () {

	kiwi:::one_of(
		list(
			(a : b : c) := {
				a + b + c
			},
			(a : b : c) := a + b + c
	))
}

kiwi_unary_variadic <- function () {

	kiwi:::one_of(
		list(
			(...) := {
				list(...)
			},
			(...) := {
				list(...)
			}
	))
}

kiwi_binary_variadic <- function () {

	kiwi:::one_of(
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
		list(id = kiwi_unary, coll = compounds$collection),
		id(coll) %is% coll
	)

	forall(
		"all forms of binary functions are functioning.",
		list(plus2 = kiwi_binary, a = atoms$integer(), b = atoms$integer()),
		plus2(a, b) == a + b
	)

	forall(
		"all forms of trinary functions are functioning.",
		list(
			plus3 = kiwi_trinary,
			a = atoms$integer(),
			b = atoms$integer(),
			c = atoms$integer()),
		plus3(a, b, c) == a + b + c
	)

message("variadic functions")

	forall(
		"all forms of variadic unary functions are functioning.",
		list(
			id = kiwi_unary_variadic,
			coll = compounds$collection),
		id(coll) %is% list(coll)
	)

	forall(
		"all forms of variadic binary functions are functioning.",
		list(
			plus2 = kiwi_binary_variadic,
			a = atoms$integer(),
			b = atoms$integer()),
		plus2(a, b) == a + b
	)
