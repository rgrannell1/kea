
# 1. Construct an kiwi object.

x_(letters)

# 2. Get data back out of an kiwi object

x_(letters) $ x_Identity()

# 3. Call methods off an kiwi object.

csv_string <- "swiss, swiss, german, irish, french, german"

parsed <- x_(csv_string) $ xExplode(", ")
freqs  <- parsed $ x_Tabulate()

# list(list("swiss", 2), list("german", 2), list("irish", 1), list("french", 1))
