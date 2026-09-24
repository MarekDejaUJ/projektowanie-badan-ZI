\name{wykres_rozklad_zerowy}
\alias{wykres_rozklad_zerowy}
\title{Rozklad zerowy z symulacji i obserwowana statystyka}
\usage{
wykres_rozklad_zerowy(
  wartosci,
  obserwowana,
  szerokosc,
  os = "Wartość statystyki",
  tytul = NULL,
  poczatek = NULL,
  dokladny = FALSE
)
}
\arguments{
\item{wartosci}{Statystyki z powtorzen przy H0.}

\item{obserwowana}{Statystyka z danych.}

\item{szerokosc}{Szerokosc przedzialu histogramu.}

\item{os}{Podpis osi wartosci.}

\item{tytul}{Tytul wykresu.}

\item{poczatek}{Poczatek pierwszego przedzialu; przy wartosciach na siatce
(pelne wyliczenie) przesuniecie o pol szerokosci centruje slupki.}

\item{dokladny}{TRUE, gdy wartosci obejmuja wszystkie mozliwe uklady;
podtytul podaje wtedy p = k / liczba ukladow.}
}
\value{
Obiekt ggplot; wykres rysuje print().
}
\description{
Histogram wartosci statystyki z B powtorzen przy H0. Przedzialy, w ktorych
|T| jest nie mniejsze od |obserwowanej|, sa wyroznione, a linie przerywane
oznaczaja obserwowana wartosc i jej odbicie. Podtytul podaje B, liczbe
wartosci co najmniej tak skrajnych i p_MC = (k + 1) / (B + 1).
}
\examples{
p <- wykres_rozklad_zerowy(c(-2, -1, -1, 0, 0, 0, 1, 1, 2), 1.5, 0.5)
}
