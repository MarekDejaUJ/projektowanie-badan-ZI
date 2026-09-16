\name{rubryka}
\alias{rubryka}
\title{Rubryka zadania lub projektu}
\usage{
rubryka(id = "PROJEKT")
}
\arguments{
\item{id}{Z01--Z10 albo PROJEKT; wielkość liter nie ma znaczenia.}
}
\value{
Lista kryteriów i obserwowalnych poziomów z punktacją.
}
\description{
Rubryka zadania lub projektu
}
\examples{
rubryka("Z07")$max_points
}
