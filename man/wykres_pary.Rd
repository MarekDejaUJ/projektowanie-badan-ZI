\name{wykres_pary}
\alias{wykres_pary}
\title{Pary pomiarow przed i po}
\usage{
wykres_pary(przed, po, id, os = "Wartość", tytul = NULL)
}
\arguments{
\item{przed, po}{Pomiary tej samej osoby w dwoch momentach.}

\item{id}{Identyfikatory osob.}

\item{os}{Podpis osi wartosci.}

\item{tytul}{Tytul wykresu.}
}
\value{
Obiekt ggplot; wykres rysuje print().
}
\description{
Kazdy odcinek laczy dwa pomiary jednej osoby; etykieta przy pomiarze
"po" podaje identyfikator i zmiane po minus przed.
}
\examples{
p <- wykres_pary(c(6, 8), c(5, 6), c("a", "b"))
}
