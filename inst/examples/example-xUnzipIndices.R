
# 1. enumerate the letters with their indices.

x_(letters) $ xUnzipIndices() $ xMap( xUnspread((ith : val) := {
	xFromWords_('the ', paste(ith), ' letter is ', val)
}) )
