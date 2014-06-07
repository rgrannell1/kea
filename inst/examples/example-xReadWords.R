
# 1. Read the last word of kiwi's description.

x_(system.file(package = 'kiwi', 'DESCRIPTION')) $ xReadWords() $ xLastOf()

# "unix"
