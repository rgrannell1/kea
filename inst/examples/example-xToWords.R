
# 1. Find the longest word in a sentence.
# searching for a minimum is a classic use of
# reduce over fold.

lorem_ipsum <-
" Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nullam gravida dui a odio
 commodo, eu porta ante pulvinar. Nulla nec adipiscing nibh. Donec sagittis ligula in
 lacus placerat, eget hendrerit justo sagittis. In varius dui blandit dui hendrerit
 mattis. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere
 cubilia Curae; Mauris at luctus erat. Aliquam eget mi vehicula, tincidunt orci vitae,
 facilisis eros. Mauris quam est, iaculis gravida mi vitae, semper lobortis magna. Donec
 mattis, ipsum vel rhoncus pretium, tortor libero accumsan lectus, vitae convallis nulla
 mauris non purus. Morbi nisi risus, rutrum condimentum sem at, tristique dictum justo.
 Duis tincidunt dictum odio ac dignissim. Sed eget auctor odio."

x_(lorem_ipsum) $
xToWords() $
x_Reduce(
	(best : new) := {
		ifelse(nchar(best) < nchar(new), new, best)
	}
)
