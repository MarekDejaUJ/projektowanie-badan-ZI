\name{wykres_histogram}
\alias{wykres_histogram}
\title{Histogram o jawnej szerokosci przedzialu}
\usage{
wykres_histogram(
  x,
  szerokosc,
  poczatek = 0,
  skala = c("liczebnosc", "gestosc"),
  os = "Wartość",
  jednostka = "min",
  tytul = NULL,
  zaznacz = NULL,
  dopasuj = character()
)
}
\arguments{
\item{x}{Wartosci liczbowe; braki sa pomijane.}

\item{szerokosc}{Szerokosc przedzialu w jednostkach x.}

\item{poczatek}{Poczatek pierwszego przedzialu.}

\item{skala}{"liczebnosc" albo "gestosc".}

\item{os}{Podpis osi wartosci.}

\item{jednostka}{Jednostka szerokosci przedzialu w podtytule.}

\item{tytul}{Tytul wykresu.}

\item{zaznacz}{Opcjonalny wektor dwoch granic; przedzialy histogramu
miedzy granicami sa wyroznione, a podtytul podaje ich udzial.}

\item{dopasuj}{Krzywe modeli: "normalny" (srednia i SD danych) lub
"lognormalny" (srednia i SD logarytmow danych).}
}
\value{
Obiekt ggplot; wykres rysuje print().
}
\description{
Przedzialy sa domkniete z prawej strony i zaczynaja sie od wartosci
`poczatek`; podtytul podaje N i szerokosc przedzialu. Skala "gestosc"
przedstawia slupki o lacznym polu 1. Opcjonalnie wyroznia przedzialy
lezace w zakresie `zaznacz` i dodaje krzywe modeli o parametrach
oszacowanych z danych.
}
\examples{
p <- wykres_histogram(c(2, 4, 5, 7, 8, 13, 30), szerokosc = 5, os = "Czas [min]")
}
