
require(arrow)

'%+%' <- arrow ::: '%+%'
throw_arrow_warning <- arrow ::: throw_arrow_warning

# -- this will be removed soon.
is_ryan <- arrow ::: is_ryan

# -- This unit test checks if the examples are empty.











r_examples <- '/home/ryan/Code/arrow.R/inst/examples'

comment_or_null <-
	xImplode_(
		'|',
		# -- is the line a comment?
		'[ 	]*[#].*$',
		# -- is the line just NULL?
		'^[ 	]*NULL[ 	]*$',
		# -- is the line empty?
		'^$'
	)











message(
	'check that every example ' %+%
	'has non-blank / NULL lines')

# -- this is awful, and should be changed.

if (is_ryan()) {

	example_lengths <-
		x_(list.files(r_examples, full.names = True)) $
		xMap(path := {

			# -- what is the file name?
			fname <-
				x_(path) $ xExplode('/') $ x_LastOf()

			# -- how many non-empty lines are there?
			len <-
				x_(path)  $ xReadLines() $
				xReject(
					xFix_(xIsMatch, comment_or_null)) $
				x_LenOf()

			list(len, fname)
		})

	empty_examples <- example_lengths $ xSelect(info := {
		xFirstOf(info) == 0
	})

	if (empty_examples $ x_NotEmpty()) {
		message <- xFromChars_(
			'the following ',
			toString(empty_examples $ x_LenOf()),
			' examples were empty; ',
			empty_examples $ xAtCol(2) $ x_Implode(', ')
		)

		throw_arrow_warning(message = message)
	}

}