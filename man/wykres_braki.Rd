\name{wykres_braki}
\alias{wykres_braki}
\title{Odsetek brakow zmiennych w grupach}
\usage{
wykres_braki(dane, zmienne, grupa, etykiety = zmienne, tytul = NULL)
}
\arguments{
\item{dane}{Ramka danych.}

\item{zmienne}{Nazwy kolumn do sprawdzenia.}

\item{grupa}{Nazwa kolumny grupujacej.}

\item{etykiety}{Czytelne nazwy zmiennych; domyslnie nazwy kolumn.}

\item{tytul}{Tytul wykresu.}
}
\value{
Obiekt ggplot; wykres rysuje print().
}
\description{
Panel dla kazdej grupy; slupek podaje odsetek brakow zmiennej, a napis
liczbe brakow i liczbe osob w grupie.
}
\examples{
d <- data.frame(g = c("a", "a", "b", "b"), x = c(1, NA, 2, 3), y = c(NA, NA, 1, 1))
p <- wykres_braki(d, c("x", "y"), "g")
}
