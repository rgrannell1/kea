
# sum a vector with fold.

xFold("+", 0, 1:10)

55

# search for a match in a list.
# as soon as a match is found, Return true
# for efficiency.

xFold(
	(acc : new) := { if (new > 100) Return (True) },
	False,
	list(99, 98, 101, 96, 94)
)

True

# demonstrate how Return works by using
# it to break out of the fold across 26 letters
# prematurely.

xFold(
	(number_checked : letter) := {

		if (letter == 'q') {
			Return (number_checked)
		} else {
			number_checked + 1
		}
	},
	0, letters
)

# terminated after 16 checks, not 26.
16

