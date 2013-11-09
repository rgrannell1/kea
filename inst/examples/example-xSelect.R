
# select the even numbers.

xSelect(n := {n %% 2 == 0}, 1:5)

list(2L, 4L)

# select names containing the letter "U".

xSelect(
	name := {
		grepl("u|U", name)
	},
	c("Church", "Turing", "Godel"))

list("Church", "Turing")

# select employees over 30.

xSelect(
	record := {
		record$age > 30
	},
	list(
		list(
			name = "Jack Thompson", age = 19),
		list(
			name = "Employee #745", age = 55),
		list(
			name = "Cog #234", age = 57)
	)
)

list(
	list(
		name = "Employee #745", age = 55),
	list(
		name = "Cog #234", age = 57)
)
