
# 1. Extracting values from tabular data. (Data from Wikipedia)

amino_acids <- x__(
	list(name = 'Glycine',       class = 'Aliphatic'),
	list(name = 'Tryptophan',    class = 'Aromatic'),
	list(name = 'Proline',       class = 'Cyclic'),
	list(name = 'Tyrosine',      class = 'Aromatic'),
	list(name = 'Phenylalanine', class = 'Aromatic'),
	list(name = 'Alanine',       class = 'Aliphatic')
)

amino_acids $ x_Map(xAtKey('class'))

# list("Aliphatic", "Aromatic", "Cyclic", "Aromatic", "Aromatic", "Aliphatic")
