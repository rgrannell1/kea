
# 1. A simple example of specialisation.
#
# xFix can be used to take a general function like add
# and create more specific instances of it; for example
# the general function 'add' can be specialised into
# increment, decrement, and addtwo.

add <- function (a, b) a + b

increment <- xFix(add, c(a = 1))

# the variadic form is often more useful.

decrement <- xFix_(add, -1)
addtwo <- xFix_(add, 2)

increment(0)

# 1

decrement(0)

# this specialisation of general functions is one of the most powerful techniques in
# functional programming.

# -1

# 2. Recursion!
#
# This may not be the most useful example, but I thought it was interesting.
# if you partially apply the Fix function itself with identity as its first argument,
# it creates the constant function: the function that takes a value and returns
# a function that always returns that value.
#
# (constant (xCapture) can be defined in easier ways though)

constant = xFix_(xFix_, xIdentity)

five <- constant(5)
five()

# 3. Avoiding Anonymous Functions
#
# Partial application cuts down the number of small anonymous functions you need.
# The following example matches every species from the genus 'Homo' (H. )
#

x_(c('H. ergaster', 'H. habilis', 'H. sapiens', 'C. elegans')) $
x_Select(xFix(xIsMatch, 'H[.] '))

# but the following is BETTER

x_(c('H. ergaster', 'H. habilis', 'H. sapiens', 'C. elegans')) $
x_Select(xIsMatch('H[.] '))


# list('H. ergaster', 'H. habilis', 'H. sapiens')

# Only use partial application like this if improves readability or
# shortens code significantly.