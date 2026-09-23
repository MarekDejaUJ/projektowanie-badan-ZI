\name{wykres_transformacje}
\alias{wykres_transformacje}
\title{Te same obserwacje w kilku skalach}
\usage{
wykres_transformacje(x, id, jednostka = "minuty", tytul = NULL)
}
\arguments{
\item{x}{Dodatnie wartosci liczbowe bez brakow.}

\item{id}{Etykiety obserwacji.}

\item{jednostka}{Nazwa jednostki surowych wartosci.}

\item{tytul}{Tytul wykresu.}
}
\value{
Obiekt ggplot.
}
\description{
Kazdy panel jest osia jednej reprezentacji (minuty, rangi, min-max,
standaryzacja z, log2); etykiety punktow wskazuja te same osoby.
}
\examples{
wykres_transformacje(c(2, 4, 4, 8), c("a", "b", "c", "d"))
}
