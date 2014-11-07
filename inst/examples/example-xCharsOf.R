
# 1. A toy example.

xCharsOf('string?')

# 7

# 2. Get the longest word in a sentence

x_("this sentence will be pulled apart and searched") $ xToWords() $ x_MaxBy(xCharsOf)

# "sentence"
