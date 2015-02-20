
# 1. Trim leading whitespace from a file.

text <- '





this is a paragraph of text
terminated by lots of whitespace.
'

x_(text) $ xToLines() $ xDropWhile(x. == '\n')

# list("this is a paragraph of text", "terminated by lots of whitespace."))
