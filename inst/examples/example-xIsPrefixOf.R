
# 1. A toy example.

xIsPrefixOf(1:3, 1:4)

# True

# 2. select paths with a certain string prefix

paths <- c('/path/dir-one/file-one', '/path/dir-one/file-two', '/path/dir-two/')

x_(paths) $ xMap(xToChars) $ xSelect(
	xIsPrefixOf(
		xToChars('/path/dir-one/') )) $
x_Map(xFromChars)

# list("/path/dir-one/file-one", "/path/dir-one/file-two")

# alternatively, you can use xIsMatch to find string matches.
# this is only a toy example; you should use .Platform $ file.sep instead of
# writing slashes into the paths directly.
