\name{xZip}
\alias{xZip}
\title{Generate a list of n-element lists from n collections.}
\usage{
  xZip(colls)
}
\arguments{
  \item{colls}{n-vectors, lists or pairlists.}
}
\value{
  returns a list of equal length to the shortest input
  collection, with each element being an n-element list.
}
\description{
  Generate a list of n-element lists from n collections.
}
\section{Corner Cases}{
  the empty list is returned if the shortest collection has
  length-zero, or no collections are included. Each
  collection is truncated to the length of the shortest
  collection.
}

