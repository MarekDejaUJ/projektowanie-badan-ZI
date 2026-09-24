\name{wykres_wielokrotne_testy}
\alias{wykres_wielokrotne_testy}
\title{Prawdopodobienstwo co najmniej jednego odrzucenia w serii testow}
\usage{
wykres_wielokrotne_testy(
  m,
  alfa = 0.05,
  etykiety = c(1, 5, 10, 20),
  tytul = NULL
)
}
\arguments{
\item{m}{Liczby testow.}

\item{alfa}{Poziom istotnosci pojedynczego testu.}

\item{etykiety}{Wartosci m opisane liczba.}

\item{tytul}{Tytul wykresu.}
}
\value{
Obiekt ggplot; wykres rysuje print().
}
\description{
Punkty 1 - (1 - alfa)^m dla m niezaleznych testow, w ktorych wszystkie
hipotezy zerowe sa prawdziwe; napisy przy wybranych m podaja wartosc.
}
\examples{
p <- wykres_wielokrotne_testy(1:20)
}
