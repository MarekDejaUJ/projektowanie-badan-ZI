\name{status_oddania}
\alias{status_oddania}
\title{Status odbioru i kontroli}
\usage{
status_oddania(id, katalog = ".", sha = NULL)
}
\arguments{
\item{id}{Z01--Z10 albo PROJEKT.}

\item{katalog}{Katalog projektu.}

\item{sha}{Opcjonalne SHA poprzedniego oddania; domyślnie lokalne HEAD.}
}
\value{
Stan odbioru; wynik kontroli Actions jest osobnym polem.
}
\description{
Status odbioru i kontroli
}
\examples{
\dontrun{ status_oddania("Z01", "moje-badania") }
}
