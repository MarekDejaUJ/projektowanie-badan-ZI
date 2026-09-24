\name{wykres_rozklady_estymatora}
\alias{wykres_rozklady_estymatora}
\title{Rozklady estymatora wobec parametru}
\usage{
wykres_rozklady_estymatora(
  srednia,
  sd,
  etykiety,
  parametr,
  os = "Wartość estymaty",
  tytul = NULL
)
}
\arguments{
\item{srednia}{Srednie rozkladow estymatorow.}

\item{sd}{Odchylenia standardowe rozkladow estymatorow.}

\item{etykiety}{Nazwy paneli.}

\item{parametr}{Wartosc parametru.}

\item{os}{Podpis osi wartosci.}

\item{tytul}{Tytul wykresu.}
}
\value{
Obiekt ggplot; wykres rysuje print().
}
\description{
Panel dla kazdego estymatora: krzywa normalna o podanej sredniej i SD
(rozklad estymat w wielu probach) oraz linia przerywana parametru.
}
\examples{
p <- wykres_rozklady_estymatora(c(10, 12), c(0.5, 0.5), c("A", "B"), 10)
}
