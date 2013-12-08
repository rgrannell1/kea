\name{xConcat}
\alias{xConcat}
\title{xConcat}
\usage{
  xConcat(colls)
}
\arguments{
  \item{colls}{several collections.}
}
\value{
  a list.
}
\description{
  Concatenate several collections into one collection.
}
\section{Corner Cases}{
  empty collections act as a unit for concatenation;
  concatenating the empty list to another list returns the
  second list, without modification.
}

