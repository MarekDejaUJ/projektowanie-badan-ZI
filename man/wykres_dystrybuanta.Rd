\name{wykres_dystrybuanta}
\alias{wykres_dystrybuanta}
\title{Dystrybuanta empiryczna z opcjonalnym modelem}
\usage{
wykres_dystrybuanta(
  x,
  prog = NULL,
  dopasuj = character(),
  os = "Wartość",
  tytul = NULL,
  grupa = NULL
)
}
\arguments{
\item{x}{Wartosci liczbowe; braki sa pomijane.}

\item{prog}{Opcjonalny prog c; punkt i napis podaja F(c).}

\item{dopasuj}{Modele: "normalny" i/lub "lognormalny".}

\item{os}{Podpis osi wartosci.}

\item{tytul}{Tytul wykresu.}

\item{grupa}{Opcjonalne etykiety grup (dlugosc jak x): osobne schodki dla
kazdej grupy, rozroznione typem linii, i wartosc F(c) kazdej grupy.}
}
\value{
Obiekt ggplot; wykres rysuje print().
}
\description{
Schodki przedstawiaja dystrybuante empiryczna: udzial obserwacji mniejszych
lub rownych wartosci na osi. Opcjonalnie oznacza wartosc dystrybuanty przy
progu i dodaje dystrybuanty modeli o parametrach oszacowanych z danych.
}
\examples{
p <- wykres_dystrybuanta(c(2, 4, 6, 8, 30), prog = 8, os = "Czas [min]")
}
