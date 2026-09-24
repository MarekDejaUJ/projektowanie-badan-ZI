\name{wykres_sd_se}
\alias{wykres_sd_se}
\title{Odchylenie standardowe i blad standardowy wobec wielkosci proby}
\usage{
wykres_sd_se(x, n, os = "Minuty", tytul = NULL)
}
\arguments{
\item{x}{Wartosci liczbowe w kolejnosci zbioru; braki sa pomijane.}

\item{n}{Wektor liczebnosci podprob (nie wiekszych od liczby waznych x).}

\item{os}{Podpis osi wartosci.}

\item{tytul}{Tytul wykresu.}
}
\value{
Obiekt ggplot; wykres rysuje print().
}
\description{
Dla kazdej liczebnosci n bierze pierwsze n waznych obserwacji zbioru,
oblicza SD osob i SE sredniej = SD / pierwiastek z n. Linia ciagla z kolami
przedstawia SD, linia przerywana z trojkatami SE.
}
\examples{
p <- wykres_sd_se(c(2, 4, 6, 8, 10, 3, 9, 5, 7, 12), c(5, 10))
}
