\name{wykres_likert}
\alias{wykres_likert}
\title{Rozklady odpowiedzi kilku pozycji porzadkowych}
\usage{
wykres_likert(
  pozycje,
  etykiety = names(pozycje),
  kategorie = 1:5,
  nazwy_kategorii = as.character(kategorie),
  tytul = NULL
)
}
\arguments{
\item{pozycje}{Ramka danych: jedna kolumna na pozycje, kody kategorii.}

\item{etykiety}{Nazwy pozycji na osi.}

\item{kategorie}{Kody kategorii w kolejnosci skali.}

\item{nazwy_kategorii}{Opisy kategorii w legendzie.}

\item{tytul}{Tytul wykresu.}
}
\value{
Obiekt ggplot; wykres rysuje print().
}
\description{
Jedna pozioma belka na pozycje; belka ma dlugosc 100% waznych odpowiedzi
pozycji, a odcinki odpowiadaja kategoriom. Napisy podaja odsetek kategorii
(od 7%). Braki sa pomijane w mianowniku kazdej pozycji.
}
\examples{
p <- wykres_likert(data.frame(p1 = c(1, 2, 4, 5, 4), p2 = c(3, 3, 4, NA, 2)))
}
