
# 1. greatest common divisor:
#    a multi parametre function.

gcd <- (a : b) := {
    if (b == 0) a else gcd(b, a %% b)
}

gcd(156, 87)

# 2. Boolean logic using only functions.

true  <- x := y := x # λx.λy.x
false <- x := y := y # λx.λy.y

# if the function is 'true', the first value True is returned.
# if the function is 'false', the second value False is returned.

to_bool <- f := f(True, False)

# now for the operators and if statement.

and <- p := q := p(q)(p)
or  <- p := q := p(p)(q)

not <- p := a := b := p(b)(a)

# if the function p is 'true', then the first
# value a is returned. Otherwise the second value is returned.
case <- p := a := b := p(a)(b)

case( and(true)(false) )('case was true')('case was false')

# 'case was false'
