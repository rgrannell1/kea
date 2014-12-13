
# 1. Construct an kea object.

x_(letters)

class(x_(letters))

# "kea"






# 2. Get data back out of an kea object.
#
# all methods are available in the form
# 'xMethod' and 'x_Method'. The former returns a kea object
# containing your value, and the latter removes the kea object
# wrapper and just gives you the value.
#
# Calling identity as an unchaining method - x_I _ takes the
# kea object containing the letters, and just returns the letters.

x_(letters) $ x_I()






# 3. Call methods off an kea object.
#
# After creating a kea object you can call methods on it using
# the dollar operator.

csv_string <- "swiss, swiss, german, irish, french, german"

# I try to add underscores to variables that are kea objects.

parsed_ <- x_(csv_string) $ xExplode(", ")
freqs  <- parsed_ $ x_Tabulate()

# list(list("swiss", 2), list("german", 2), list("irish", 1), list("french", 1))






# 4. Chaining methods.
#
# There isn't an advantage to using methods when you only need to apply
# one or two functions to a datum. But you should use methods when you
# need to apply lots of functions to a single datum.
#
# really nested function calls are hard to read, but long chains of
# functions are easy to read.

# good

x_(letters) $ xMap(toupper) $ xRepeat(10) $ xShuffle() $ xTake(30) $ x_Tabulate()

# very bad

xTabulate(
	xTake(30,
		xShuffle(
			xRepeat(10,
				xMap(toupper, letters)
			)
		)
	)
)
