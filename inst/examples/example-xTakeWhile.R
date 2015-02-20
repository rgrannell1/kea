
# 1. Trim trailing whitespace from a file.

text <- '
this is a paragraph of text
terminated by lots of whitespace.





'

x_(text) $ xToLines() $ xTakeWhile(x. != '\n')

# list("this is a paragraph of text", "terminated by lots of whitespace."))
