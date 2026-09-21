\name{status_oddania}
\alias{status_oddania}
\title{Status odbioru i kontroli}
\usage{
status_oddania(id, katalog = NULL, sha = NULL)
}
\arguments{
\item{id}{Z01--Z10 albo PROJEKT.}

\item{katalog}{Katalog własnej przestrzeni; NULL rozpoznaje bieżącą pracę.}

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
