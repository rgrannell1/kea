
# 1. Test if a string matches a regular expression

xIsMatch('Over 90+[!]$', 'Over 9000!')

# True

# 2. Select matching strings from a list

text <-
"R version 3.1.1 (2014-07-10) -- 'Sock it to Me'
Copyright (C) 2014 The R Foundation for Statistical Computing
Platform: x86_64-pc-linux-gnu (64-bit)

R is free software and comes with ABSOLUTELY NO WARRANTY.
You are welcome to redistribute it under certain conditions.
Type 'license()' or 'licence()' for distribution details.

  Natural language support but running in an English locale

R is a collaborative project with many contributors.
Type 'contributors()' for more information and
'citation()' on how to cite R or R packages in publications."

x_(text) $ xToLines() $ x_Select(xIsMatch('licence|citation'))

# list(
#      "Type 'license()' or 'licence()' for distribution details.",
#      "'citation()' on how to cite R or R packages in publications.")
