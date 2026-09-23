\name{wykres_srednia_mediana}
\alias{wykres_srednia_mediana}
\title{Pomiary osob ze srednia i mediana}
\usage{
wykres_srednia_mediana(x, grupa = NULL, os = "Wartość", tytul = NULL)
}
\arguments{
\item{x}{Wartosci liczbowe; braki sa pomijane.}

\item{grupa}{Opcjonalne etykiety grup lub serii.}

\item{os}{Podpis osi wartosci.}

\item{tytul}{Tytul wykresu.}
}
\value{
Obiekt ggplot; wykres rysuje print().
}
\description{
Kazda osoba jest kolem na osi wartosci; romb oznacza srednia, a pionowa
kreska mediane grupy. Rozne ksztalty pozwalaja odczytac wykres bez koloru.
}
\examples{
p <- wykres_srednia_mediana(c(2, 3, 3, 20), os = "Czas [min]")
}
