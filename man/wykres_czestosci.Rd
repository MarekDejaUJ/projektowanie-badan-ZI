\name{wykres_czestosci}
\alias{wykres_czestosci}
\title{Czestosci kategorii z odsetkiem skumulowanym}
\usage{
wykres_czestosci(kategorie, liczebnosc, os = "Kategoria", tytul = NULL)
}
\arguments{
\item{kategorie}{Etykiety kategorii w kolejnosci skali.}

\item{liczebnosc}{Liczby odpowiedzi w kategoriach.}

\item{os}{Podpis osi kategorii.}

\item{tytul}{Tytul wykresu.}
}
\value{
Obiekt ggplot; wykres rysuje print().
}
\description{
Slupki podaja odsetek kazdej kategorii (napis: liczebnosc i odsetek), a
linia z trojkatami odsetek skumulowany do danej kategorii wlacznie.
Odsetek skumulowany ma sens dla kategorii uporzadkowanych.
}
\examples{
p <- wykres_czestosci(1:5, c(2, 3, 5, 7, 3), os = "Odpowiedz")
}
