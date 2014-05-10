
# Grammar

# it(str):                       add a description (singleton field).
# over(...symbols):              give the parametres to be bound (singleton field).

# check(expr):                   add a single predicate to test.
# checkWhen(expr, expr):         add a single predicate to test of a subset of the domain.

# fails(expr, [str]):            add a single function to test for expected failure.
# failsWhen(expr, expr, [str]):  add a single function to test for expected failure, over a limited domain.

# select(expr):                  select a subset of the stream.

# chain with '|': it('this is a test') | over(x, y)
#

# -- the description

it <- function () {

}

# -- the domain over which to bind

over <- function () {

}

# -- test properties (+ controls)

check <- function () {

}

checkWhen <- function () {

}

# -- test failures (- controls)

fails <- function () {

}

failsWhen <- function () {

}
