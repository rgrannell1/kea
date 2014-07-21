
# 1. A simple set membership example.

is_primary_colour <- is_primary_color <- val := {
	xIsIn_(val, 'red', 'green', 'blue')
}

is_primary_colour('red')

# True

is_primary_colour('magenta')

# False
