
# 1. Use xLocate like xWhich

xLocate(xI, c(True, False, True, Na))

# c(1, 3)

# 2. Locate the solutions to a polynomial

solves_equation <- x := {
	x^2 - 6*x == 0
}

x_(-10:10) $ xLocate(solves_equation)

# the 11th and 17th elements of -10, -9, ..., 10 solve the equation. (x = 0, x = 6)
