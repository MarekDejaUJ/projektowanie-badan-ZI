\name{wykres_roznica_zmian}
\alias{wykres_roznica_zmian}
\title{Roznica zmian w dwoch grupach}
\usage{
wykres_roznica_zmian(przed, po, grupy, kontrfakt, os = "Wartość", tytul = NULL)
}
\arguments{
\item{przed, po}{Dwuelementowe wektory srednich przed i po zmianie.}

\item{grupy}{Nazwy dwoch grup; pierwsza grupa przechodzi interwencje.}

\item{kontrfakt}{Nazwa linii przebiegu przy trendzie drugiej grupy.}

\item{os}{Podpis osi wartosci.}

\item{tytul}{Tytul wykresu.}
}
\value{
Obiekt ggplot.
}
\description{
Linie lacza pomiar przed i po w dwoch grupach. Linia przerywana pokazuje
przebieg pierwszej grupy przy zalozeniu zmiany rownej zmianie grupy drugiej.
}
\examples{
wykres_roznica_zmian(c(10, 8), c(7, 7), c("Filia A", "Filia B"), "A przy trendzie B")
}
