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
  tytul = NULL
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
}
\value{
Obiekt ggplot.
}
\description{
Przedzialy zaczynaja sie od wartosci `poczatek`; podtytul podaje N i
szerokosc przedzialu. Skala "gestosc" przedstawia pole slupkow rowne 1.
}
\examples{
wykres_histogram(c(2, 4, 5, 7, 8, 13, 30), szerokosc = 5, os = "Czas [min]")
}
